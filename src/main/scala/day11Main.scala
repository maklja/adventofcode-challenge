import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11Challenge:

  enum GalaxySymbol(value: Char):
    case Galaxy extends GalaxySymbol('#')
    case Void extends GalaxySymbol('.')

    def getValue() = value

  object GalaxySymbol:
    def mapToValue(value: Char) = value match
      case '#' => GalaxySymbol.Galaxy
      case _   => GalaxySymbol.Void

  end GalaxySymbol

  case class Position(x: Int, y: Int)

  case class GalaxyField(symbol: GalaxySymbol, position: Position, dx: Int, dy: Int)

  class GalaxyMap(val galaxyData: Seq[String]):
    val galaxyFields = createGalaxyFields().groupBy(_.position.x)

    val galaxies =
      galaxyFields.values.flatten.filter(galaxyField => galaxyField.symbol == GalaxySymbol.Galaxy).toSeq

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
          .filter(!_._1.contains(GalaxySymbol.Galaxy.getValue()))
          .map(_._2)

      val emptyColumns = galaxyData
        .foldLeft(Seq[String]())((columns, row) =>
          row.zipWithIndex.map(iChar => columns.lift(iChar._2).getOrElse("") :+ iChar._1)
        )
        .zipWithIndex
        .filter(!_._1.contains(GalaxySymbol.Galaxy.getValue()))
        .map(_._2)

      galaxyData.zipWithIndex
        .flatMap(iGalaxyRow => {
          val rowValue = if (emptyRows.contains(iGalaxyRow._2)) 2 else 1
          iGalaxyRow._1.zipWithIndex.map(iGalaxy => {
            val colValue = if (emptyColumns.contains(iGalaxy._2)) 2 else 1
            val p = Position(iGalaxyRow._2, iGalaxy._2)

            GalaxyField(
              GalaxySymbol.mapToValue(iGalaxy._1),
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
    val pathValues = galaxyCombinations.map(curCombination => {
      val p1 = curCombination(0)
      val p2 = curCombination(1)

      galaxyMap
        .getGalaxyFields(p1.position, p2.position)
        .foldLeft((0, Option.empty[GalaxyField]))((pathValue, galaxyField) => {
          val (value, prevGalaxyField) = pathValue
          if (prevGalaxyField.isEmpty) {
            (value, Some(galaxyField))
          } else if (prevGalaxyField.get.position.x == galaxyField.position.x) {
            (
              value + prevGalaxyField.get.dy,
              Some(galaxyField)
            )
          } else {
            (
              value + prevGalaxyField.get.dx,
              Some(galaxyField)
            )
          }
        })
        ._1
    })

    pathValues.sum

  @main def day11Main(): Unit =
    Using.Manager { use =>

      val galaxyData =
        use(Source.fromResource("day11/input.txt")).getLines().toSeq
      val galaxyMap = GalaxyMap(galaxyData)

      val pathValuesSum = calculateGalaxyDistance(galaxyMap)
      println(f"Galaxy path values sum: ${pathValuesSum}")
    }

end Day11Challenge
