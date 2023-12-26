import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11Challenge:
  val galaxySymbol = '#'
  val emptySymbol = '.'

  case class Position(x: Int, y: Int)

  case class GalaxyField(symbol: Char, position: Position, dx: Int, dy: Int)

  class GalaxyMap(val galaxyData: Seq[String]):
    val galaxyFields = createGalaxyFields().groupBy(_.position.x)

    val galaxies =
      galaxyFields.values.flatten.filter(galaxyField => galaxyField.symbol == galaxySymbol).toSeq

    def getGalaxyFields(p1: Position, p2: Position) =
      val minX = Math.min(p1.x, p2.x)
      val maxX = Math.max(p1.x, p2.x)
      val minY = Math.min(p1.y, p2.y)
      val maxY = Math.max(p1.y, p2.y)

      val rowGalaxyFields = Range.inclusive(minX, maxX).map(i => galaxyFields(i)(minY))
      val columnGalaxyFields = Range.inclusive(minY + 1, maxY).map(i => galaxyFields(maxX)(i))

      rowGalaxyFields ++ columnGalaxyFields

    private def createGalaxyFields() =
      val emptyRows =
        galaxyData.zipWithIndex
          .filter(!_._1.contains(galaxySymbol))
          .map(_._2)

      val emptyColumns = galaxyData
        .foldLeft(Seq[String]())((columns, row) =>
          row.zipWithIndex.map(iChar => columns.lift(iChar._2).getOrElse("") :+ iChar._1)
        )
        .zipWithIndex
        .filter(!_._1.contains(galaxySymbol))
        .map(_._2)

      galaxyData.zipWithIndex
        .flatMap(iGalaxyRow => {
          val rowValue = if (emptyRows.contains(iGalaxyRow._2)) 2 else 1
          iGalaxyRow._1.zipWithIndex.map(iGalaxy => {
            val colValue = if (emptyColumns.contains(iGalaxy._2)) 2 else 1
            val p = Position(iGalaxyRow._2, iGalaxy._2)
            GalaxyField(
              iGalaxy._1,
              p,
              rowValue,
              colValue
            )
          })
        })
        .sortBy(_.position.y)

  end GalaxyMap

  def calculateGalaxyDistance(galaxyMap: GalaxyMap) =
    val galaxyCombinations = galaxyMap.galaxies.combinations(2).toSeq
    println(galaxyMap.getGalaxyFields(Position(2, 2), Position(1, 1)))
    println(galaxyCombinations.length)

  @main def day11Main(): Unit =
    Using.Manager { use =>

      val galaxyData =
        use(Source.fromResource("day11/smallInput.txt")).getLines().toSeq
      val galaxyMap = GalaxyMap(galaxyData)

      calculateGalaxyDistance(galaxyMap)
    }

end Day11Challenge
