import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12Challenge:

  enum SpringStatus(value: Char):
    case Operational extends SpringStatus('.')
    case Malfunction extends SpringStatus('#')
    case Unknown extends SpringStatus('?')

    def getValue() = value

    def getValueStr() =
      value match
        case '.' => "\\."
        case '#' => "#"
        case '?' => "\\?"

  case class SpringRow(springsStatus: String, springsCount: Seq[Int])

  case class SpringRowRange(springStatusChunk: String, index: Int):
    val unknownCount = springStatusChunk.count(spring => spring == SpringStatus.Unknown.getValue())

    def replaceUnknown(replaceCount: Int): Option[SpringRowRange] =
      if (unknownCount < replaceCount) {
        return None
      }

      val replacedSpringStatusChunk = Range(0, replaceCount).foldLeft(springStatusChunk)((springStatus, i) => {
        val unknownSymbol = SpringStatus.Unknown.getValueStr()
        val malfunctionSymbol = SpringStatus.Malfunction.getValueStr()
        springStatus.replaceFirst(unknownSymbol, malfunctionSymbol)
      })

      Some(SpringRowRange(replacedSpringStatusChunk, index))

    def createCombinations(): Seq[String] =

      @tailrec
      def rotateCombination(
          springRowStatus: String,
          rotateNum: Int,
          results: Seq[String]
      ): Seq[String] =
        if (rotateNum <= 0) {
          return results
        }

        val rotateSpringRow = springRowStatus.last +: springRowStatus.slice(0, springRowStatus.length - 1)
        rotateCombination(rotateSpringRow, rotateNum - 1, results :+ rotateSpringRow)

      val unknownSymbol = SpringStatus.Unknown.getValueStr()
      val operationalSymbol = SpringStatus.Operational.getValueStr()
      val unknownCount = springStatusChunk.count(spring => spring == SpringStatus.Unknown.getValue())

      val populatedSpringRow = springStatusChunk.replaceAll(unknownSymbol, operationalSymbol)
      rotateCombination(populatedSpringRow, unknownCount, Seq(populatedSpringRow))

  def parseSpringData(springData: Seq[String]) =
    springData.map(curSpringData => {
      val springsAndCounts = curSpringData.split(" ")
      val springs = springsAndCounts(0)
      val count = springsAndCounts(1).split(",").map(_.toInt).toSeq

      SpringRow(springs, count)
    })

  def createSpringRowRange(springRows: Seq[SpringRow]) =

    @tailrec
    def nextSpringRowRange(
        springsStatus: String,
        springIndex: Int = 0,
        results: Seq[SpringRowRange] = Seq()
    ): Seq[SpringRowRange] =
      val remainingSpringsStatus = springsStatus.dropWhile(spring => spring == SpringStatus.Operational.getValue())
      if (remainingSpringsStatus.isEmpty()) {
        return results
      }

      val springRange = remainingSpringsStatus.takeWhile(spring => spring != SpringStatus.Operational.getValue())

      val springRowRange = SpringRowRange(springRange, springIndex)
      nextSpringRowRange(
        remainingSpringsStatus.slice(springRange.length(), remainingSpringsStatus.length()),
        springIndex + 1,
        results :+ springRowRange
      )

    springRows.map(springRow => (springRow, nextSpringRowRange(springRow.springsStatus)))

  // def splitSpringRowRange(springRowCount: Seq[Int], springRowRange: SpringRowRange) =

  def calcCombinations(springRow: SpringRow, springRowRanges: Seq[SpringRowRange]) =

    if (springRowRanges.length < springRow.springsCount.length) {
      // TODO split
    } else {
      val curSpringRowRange = springRowRanges.head
      val curSpringRowCount = springRow.springsCount.head

      println(curSpringRowRange.replaceUnknown(curSpringRowCount).get.createCombinations())
    }
    ""

  @main def day12Main(): Unit =
    Using.Manager { use =>

      val springsData =
        use(Source.fromResource("day12/smallInput.txt")).getLines().toSeq
      val springRows = parseSpringData(springsData)
      createSpringRowRange(springRows).foreach(x => {

        calcCombinations(x._1, x._2)
      })
    }

end Day12Challenge
