import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day10Challenge:

  enum WayDirection(value: Int):
    case Up extends WayDirection(1)
    case Down extends WayDirection(-1)
    case None extends WayDirection(0)

    def getValue() = value
  end WayDirection

  enum Direction(value: Char):
    case North extends Direction('n')
    case South extends Direction('s')
    case West extends Direction('w')
    case East extends Direction('e')
    case Start extends Direction('s')
  end Direction

  enum PipeSymbol(value: Char):
    case NorthSouth extends PipeSymbol('|')
    case WesEast extends PipeSymbol('-')
    case NorthEast extends PipeSymbol('L')
    case NorthWest extends PipeSymbol('J')
    case SouthWest extends PipeSymbol('7')
    case SouthEast extends PipeSymbol('F')
    case Ground extends PipeSymbol('.')
    case StartPosition extends PipeSymbol('S')

    def getValue() = value
  end PipeSymbol

  val northMap = Map(
    (PipeSymbol.NorthSouth -> Direction.North),
    (PipeSymbol.SouthEast -> Direction.East),
    (PipeSymbol.SouthWest -> Direction.West),
    (PipeSymbol.StartPosition -> Direction.Start)
  )

  val southMap = Map(
    (PipeSymbol.NorthSouth -> Direction.South),
    (PipeSymbol.NorthWest -> Direction.West),
    (PipeSymbol.NorthEast -> Direction.East),
    (PipeSymbol.StartPosition -> Direction.Start)
  )

  val westMap = Map(
    (PipeSymbol.WesEast -> Direction.West),
    (PipeSymbol.NorthEast -> Direction.North),
    (PipeSymbol.SouthEast -> Direction.South),
    (PipeSymbol.StartPosition -> Direction.Start)
  )

  val eastMap = Map(
    (PipeSymbol.WesEast -> Direction.East),
    (PipeSymbol.NorthWest -> Direction.North),
    (PipeSymbol.SouthWest -> Direction.South),
    (PipeSymbol.StartPosition -> Direction.Start)
  )

  val directionsMap = Map(
    (Direction.North -> northMap),
    (Direction.South -> southMap),
    (Direction.West -> westMap),
    (Direction.East -> eastMap)
  )

  case class Position(x: Int, y: Int)

  case class PipePart(
      position: Position,
      symbol: PipeSymbol,
      wayDirection: WayDirection = WayDirection.None
  )

  class PipesMap(pipesMap: Seq[String]):
    val symbolPipesMap = pipesMap.map(mapRow =>
      mapRow
        .map(symbol => PipeSymbol.values.find(_.getValue() == symbol).get)
        .toSeq
    )

    def getRowPositions(rowIdx: Int): Seq[Position] =
      symbolPipesMap(rowIdx).zipWithIndex.map(indexedSymbol =>
        Position(rowIdx, indexedSymbol._2)
      )

    def getSymbol(p: Position): PipeSymbol =
      if (p.x < 0 || p.y < 0) {
        PipeSymbol.Ground
      } else {
        symbolPipesMap(p.x)(p.y)
      }

    def positionFromDirection(
        p: Position,
        direction: Direction
    ): Option[Position] =
      direction match
        case Direction.North => Some(Position(p.x - 1, p.y))
        case Direction.South => Some(Position(p.x + 1, p.y))
        case Direction.West  => Some(Position(p.x, p.y - 1))
        case Direction.East  => Some(Position(p.x, p.y + 1))
        case _               => None

    def findStartPosition(): Option[Position] =
      symbolPipesMap.zipWithIndex.find(mapRow =>
        mapRow._1.contains(PipeSymbol.StartPosition)
      ) match {
        case Some(mapRow) =>
          Some(Position(mapRow._2, mapRow._1.indexOf(PipeSymbol.StartPosition)))
        case _ => None
      }
  end PipesMap

  // return (current symbol way direction value, current way direction value)
  def calcNextWayDirection(
      direction: Direction,
      symbol: PipeSymbol,
      curWayDirection: WayDirection
  ): (WayDirection, WayDirection) =
    if (symbol == PipeSymbol.WesEast) {
      return (WayDirection.None, curWayDirection)
    }

    if (direction == Direction.North) {
      (WayDirection.Up, WayDirection.Up)
    } else if (direction == Direction.South) {
      (WayDirection.Down, WayDirection.Down)
    } else {
      (curWayDirection, curWayDirection)
    }

  @tailrec
  def searchPath(
      p: Position,
      direction: Direction,
      map: PipesMap,
      wayDirection: WayDirection = WayDirection.Up,
      path: Seq[PipePart] = Seq()
  ): Option[Seq[PipePart]] =

    val allowedNextSymbols = directionsMap(direction)
    map
      // get next position by direction
      .positionFromDirection(p, direction)
      // get symbol for next position
      .map(nextPosition => (nextPosition, map.getSymbol(nextPosition)))
      // ensure that we can transition from current symbol to the next symbol
      .filter(nextPipe => allowedNextSymbols.keySet.contains(nextPipe._2)) match
      case Some(nextPipe) => {
        val wayDirections =
          calcNextWayDirection(direction, nextPipe._2, wayDirection)
        val pipePart = PipePart(p, map.getSymbol(p), wayDirections._1)
        if (nextPipe._2 == PipeSymbol.StartPosition) {
          return Some(path :+ pipePart)
        }

        val nextDirection = allowedNextSymbols(nextPipe._2)
        searchPath(
          nextPipe._1,
          nextDirection,
          map,
          wayDirections._2,
          path :+ pipePart
        )
      }
      case _ => None

  @tailrec
  def searchPipes(
      positions: Seq[Position],
      directions: Seq[Direction],
      map: PipesMap
  ): Option[Seq[PipePart]] =
    if (directions.isEmpty) {
      return None
    }

    val curDirection = directions.head
    val curPosition = positions.last

    searchPath(curPosition, curDirection, map) match
      case Some(path) => Some(path)
      case _          => searchPipes(positions, directions.tail, map)

  def pipelinePath(pipesMap: PipesMap): Seq[PipePart] =
    val startPosition = pipesMap.findStartPosition() match {
      case Some(position) => position
      case _ => throw new RuntimeException("Starting position not found")
    }

    searchPipes(
      Seq(startPosition),
      Seq(Direction.North, Direction.South, Direction.West, Direction.East),
      pipesMap
    ).getOrElse(Seq())

  def findNests(path: Seq[PipePart], pipesMap: PipesMap) =
    val pathPositions = path.map(_.position)
    // all positions that are not in the path
    val possibleNestPositions = pipesMap.symbolPipesMap.zipWithIndex
      .flatMap(indexedRow =>
        indexedRow._1.zipWithIndex
          .map(iSym => Position(indexedRow._2, iSym._2))
          .filter(symPosition => !pathPositions.contains(symPosition))
      )

    // find nests inside path
    possibleNestPositions.filter(nestPosition => {
      val pipeParts = path
        // get all path to the right of the nestPosition
        .filter(pathPart => {
          pathPart.position.x == nestPosition.x && pathPart.position.y > nestPosition.y && pathPart.wayDirection
            .getValue() != 0
        })
        // sort by columns
        .sortBy(_.position.y)
        .map(_.wayDirection.getValue())
        // eliminate duplicate way direction values, ex. 1, -1, -1 => 1, -1
        .foldLeft(Seq[Int]())((wayDirections, wayDirection) => {
          if (wayDirections.isEmpty || wayDirections.last != wayDirection) {
            wayDirections :+ wayDirection
          } else {
            wayDirections
          }
        })
        // see if nest is inside or outside of the path
        // 0 => it is outside of the path
        // -1 or 1 => it is inside of the path
        .reduceOption((wayValue1, wayValue2) => {
          if (wayValue1 == 0) {
            wayValue2
          } else if (wayValue1 == wayValue2) {
            wayValue1
          } else {
            0
          }
        })
        .getOrElse(0)

      pipeParts != 0
    })

  @main def day10Main(): Unit =
    Using.Manager { use =>

      val pipeData =
        use(Source.fromResource("day10/input.txt")).getLines().toSeq
      val map = new PipesMap(pipeData)
      val path = pipelinePath(map)

      val path1 = path.tail.map(_.position).zipWithIndex
      val path2 = path.tail.reverse.map(_.position).zipWithIndex
      val farthestPipelinePoint = path1.intersect(path2).map(_._2).head + 1
      println(f"Farthest pipeline point: ${farthestPipelinePoint}")

      val nestLocations = findNests(path, map)
      println(f"Nests inside a path: ${nestLocations.length}")
    }

end Day10Challenge
