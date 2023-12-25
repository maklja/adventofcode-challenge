import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day11Challenge:
  val galaxySymbol = '#'
  val emptySymbol = '.'

  @main def day11Main(): Unit =

    case class GalaxyField(symbol: Char, x: Int, y: Int, dx: Int, dy: Int)

    class GalaxyMap(val galaxyData: Seq[String]):
      val galaxyFields = createGalaxyFields()

      private def createGalaxyFields() =
        val emptyRows =
          galaxyData.zipWithIndex
            .filter(!_._1.contains(galaxySymbol))
            .map(_._2)

        val emptyColumns = galaxyData
          .foldLeft(Seq[String]())((columns, row) =>
            row.zipWithIndex.map(iChar =>
              columns.lift(iChar._2).getOrElse("") :+ iChar._1
            )
          )
          .zipWithIndex
          .filter(!_._1.contains(galaxySymbol))
          .map(_._2)

        galaxyData.zipWithIndex
          .flatMap(iGalaxyRow => {
            val rowValue = if (emptyRows.contains(iGalaxyRow._2)) 2 else 1
            iGalaxyRow._1.zipWithIndex.map(iGalaxy => {
              val colValue = if (emptyColumns.contains(iGalaxy._2)) 2 else 1
              GalaxyField(
                iGalaxy._1,
                iGalaxyRow._2,
                iGalaxy._2,
                rowValue,
                colValue
              )
            })
          })

    end GalaxyMap

    Using.Manager { use =>

      val galaxyData =
        use(Source.fromResource("day11/smallInput.txt")).getLines().toSeq
      val galaxyMap = GalaxyMap(galaxyData)

      println(galaxyMap.galaxyFields)
    }

end Day11Challenge
