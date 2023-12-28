import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day12Challenge:

  enum SpringStatus(value: Char):
    case Operational extends SpringStatus('.')
    case Malfunction extends SpringStatus('#')
    case Unknown extends SpringStatus('?')
    case Marked extends SpringStatus('*')

    def getValue() = value

    def getValueStr() =
      value match
        case '.' => "\\."
        case '#' => "#"
        case '?' => "\\?"
        case '*' => "*"

  case class SpringRow(springsStatus: String, springsCount: Seq[Int])

  case class SpringRowRange(springStatusChunk: String, index: Int):

    def countUnknownSpots() = springStatusChunk.count(_ == SpringStatus.Unknown.getValue())

    def countMarkedSpots() = springStatusChunk.count(_ == SpringStatus.Marked.getValue())

    def fillOperationalSpots(): SpringRowRange =
      SpringRowRange(springStatusChunk.replaceAll("\\*\\?\\*", "\\*\\.\\*"), index)

    def markUnknownSpot(replaceCount: Int): Option[SpringRowRange] =
      val lastMarkedSpot = springStatusChunk.lastIndexOf(SpringStatus.Marked.getValue())
      val nextSpotToMark = if (lastMarkedSpot == -1) 0 else lastMarkedSpot + 2
      val availableSpots = springStatusChunk.slice(nextSpotToMark, springStatusChunk.length())
      if (availableSpots.length() < replaceCount) {
        return None
      }

      val markedSpots =
        springStatusChunk.patch(nextSpotToMark, SpringStatus.Marked.getValueStr().repeat(replaceCount), replaceCount)

      Some(SpringRowRange(markedSpots, index))

  def parseSpringData(springData: Seq[String]) =
    springData.map(curSpringData => {
      val springsAndCounts = curSpringData.split(" ")
      val springs = springsAndCounts(0)
      val count = springsAndCounts(1).split(",").map(_.toInt).toSeq

      SpringRow(springs, count)
    })

  def createSpringRowRange(springRows: Seq[SpringRow]) =

    def rotateSpringRowRange(springRow: SpringRowRange): SpringRowRange =
      val firstMalfunctionIdx = springRow.springStatusChunk.indexOf(SpringStatus.Malfunction.getValueStr())
      if (firstMalfunctionIdx == -1) {
        return springRow
      }

      val beginPart = springRow.springStatusChunk.take(firstMalfunctionIdx)
      val endPart = springRow.springStatusChunk.slice(firstMalfunctionIdx, springRow.springStatusChunk.length)

      SpringRowRange(endPart + beginPart, springRow.index)

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

    springRows.map(springRow => (springRow, nextSpringRowRange(springRow.springsStatus).map(rotateSpringRowRange)))

  def markSpringRowRanges(springStatusCount: Seq[Int], springRowRanges: Seq[SpringRowRange]): Seq[SpringRowRange] =

    @tailrec
    def markNextSpringRows(
        springStatusCount: Seq[Int],
        springRowRanges: Seq[SpringRowRange],
        markedSpringRowRanges: Seq[SpringRowRange] = Seq()
    ): Seq[SpringRowRange] =
      if (springStatusCount.isEmpty) {
        return markedSpringRowRanges ++ springRowRanges;
      }

      if (springRowRanges.isEmpty) {
        throw new RuntimeException(f"Invalid spring row ranges for sequence ${springStatusCount}")
      }

      val springRowRange = springRowRanges.head
      val statusCount = springStatusCount.head

      springRowRange.markUnknownSpot(statusCount) match
        case None =>
          markNextSpringRows(springStatusCount, springRowRanges.tail, markedSpringRowRanges :+ springRowRange)
        case Some(newSpringStatusRange) =>
          markNextSpringRows(
            springStatusCount.tail,
            newSpringStatusRange +: springRowRanges.tail,
            markedSpringRowRanges
          )

    markNextSpringRows(springStatusCount, springRowRanges).map(_.fillOperationalSpots())

  def calculateCombinations(markedSpringRowRanges: Seq[SpringRowRange]): Int =
    // combinations formula C(n, k) = n! / k!(n - k)!
    // 6 / 2 => 3
    @tailrec
    def factorial(n: Int, sum: Int = 1): Int =
      if (n <= 1) {
        return sum
      }

      factorial(n - 1, sum * n)

    markedSpringRowRanges
      .map(springRange => {
        val totalSprings = springRange.countMarkedSpots()

        val unknownFieldsCount = springRange.countUnknownSpots()
        println(f"${totalSprings} = ${unknownFieldsCount}")
        if (unknownFieldsCount == 0) {
          0
        } else {
          factorial(totalSprings) / factorial(unknownFieldsCount) * factorial(totalSprings - unknownFieldsCount)
        }
      })
      .sum

  @main def day12Main(): Unit =
    Using.Manager { use =>

      val springsData =
        use(Source.fromResource("day12/smallInput.txt")).getLines().toSeq
      val springRows = parseSpringData(springsData)

      val markedSpringRowRanges = createSpringRowRange(springRows).map((springRowRangeTuple) => {
        val (springRow, springROwRanges) = springRowRangeTuple
        markSpringRowRanges(springRow.springsCount, springROwRanges)
      })

      println(markedSpringRowRanges.map(calculateCombinations))
    }

    // 4, 1
    // 24
    // 1 * 3 * 2 => 6 4

end Day12Challenge
