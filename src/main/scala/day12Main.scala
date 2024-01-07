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
        case '.' => """."""
        case '#' => "#"
        case '?' => """?"""
        case '*' => "*"

  case class SpringRow(springsStatus: String, springsCount: Seq[Int])

  case class SpringRange(count: Int, range: Range)

  case class SpringRowRange(
      springStatusChunk: String,
      ranges: Seq[SpringRange] = Seq()
  )

  def parseSpringData(springData: Seq[String]) =
    springData.map(curSpringData => {
      val springsAndCounts = curSpringData.split(" ")
      val springs = springsAndCounts(0)
      val count = springsAndCounts(1).split(",").map(_.toInt).toSeq

      SpringRow(springs, count)
    })

  def splitSpringRowIntoRanges(springRows: Seq[SpringRow]) =

    @tailrec
    def splitSpringRow(
        springsStatus: String,
        results: Seq[SpringRowRange] = Seq()
    ): Seq[SpringRowRange] =
      val remainingSpringsStatus = springsStatus.dropWhile(spring => spring == SpringStatus.Operational.getValue())
      if (remainingSpringsStatus.isEmpty()) {
        return results
      }

      val springRange = remainingSpringsStatus.takeWhile(spring => spring != SpringStatus.Operational.getValue())
      val range = SpringRowRange(springRange)
      splitSpringRow(
        remainingSpringsStatus.slice(springRange.length(), remainingSpringsStatus.length()),
        results :+ range
      )

    springRows.map(springRow => (springRow, splitSpringRow(springRow.springsStatus)))

  @tailrec
  def markSpringStatusCount(
      springStatusCount: Int,
      range: SpringRowRange,
      markPosition: Int = 0
  ): Option[(Int, SpringRowRange)] =
    val availableSpots = range.springStatusChunk.slice(markPosition, range.springStatusChunk.length())
    if (availableSpots.length() < springStatusCount) {
      return None
    }

    val markRange = Range(markPosition, Math.min(range.springStatusChunk.length(), markPosition + springStatusCount))
    val markedSpots =
      markRange
        .foldLeft(range.springStatusChunk)((springStatus, i) => {
          if (springStatus(i) == SpringStatus.Malfunction.getValue()) {
            springStatus
          } else {
            springStatus.patch(i, SpringStatus.Marked.getValueStr(), 1)
          }
        })

    val countCheck = markedSpots
      .slice(markRange.head, markRange.head + markRange.length + 1)
      .count(status => status != SpringStatus.Unknown.getValue())
    if (countCheck == springStatusCount) {
      val maxRange =
        markedSpots.zipWithIndex
          .slice(markRange.head, markRange.head + markRange.length)
          .find(_._1 == SpringStatus.Malfunction.getValue())
          .map(_._2 + springStatusCount)
          .getOrElse(range.springStatusChunk.length())
      val nextSpringRange =
        SpringRange(springStatusCount, Range(markRange.head, maxRange))
      val springRowRanges = range.ranges.reverse
        .foldLeft(Seq(nextSpringRange))((newRanges, curRange) => {
          val updatedMaxRange = Math.min(curRange.range.end, newRanges.last.range.last - 1)
          val updatedCurRange = Range(curRange.range(0), updatedMaxRange)
          newRanges :+ curRange.copy(range = updatedCurRange)
        })
        .reverse

      Some(
        markPosition + markRange.length + 1,
        SpringRowRange(
          markedSpots,
          springRowRanges
        )
      )
    } else {
      markSpringStatusCount(springStatusCount, range, markPosition + 1)
    }

  def markSpringRowRanges(springStatusCount: Seq[Int], springRowRanges: Seq[SpringRowRange]): Seq[SpringRowRange] =

    @tailrec
    def markNextSpringRows(
        springStatusCount: Seq[Int],
        ranges: Seq[SpringRowRange],
        markPosition: Int = 0,
        markedSpringRowRanges: Seq[SpringRowRange] = Seq()
    ): Seq[SpringRowRange] =
      if (springStatusCount.isEmpty) {
        return markedSpringRowRanges ++ ranges
      }

      if (ranges.isEmpty) {
        throw new RuntimeException(f"Invalid spring row ranges for sequence ${springStatusCount}")
      }

      val springRowRange = ranges.head
      val statusCount = springStatusCount.head

      markSpringStatusCount(statusCount, springRowRange, markPosition) match
        case None =>
          markNextSpringRows(springStatusCount, ranges.tail, 0, markedSpringRowRanges :+ springRowRange)
        case Some((nextMarkPosition, newSpringStatusRange)) =>
          markNextSpringRows(
            springStatusCount.tail,
            newSpringStatusRange +: ranges.tail,
            nextMarkPosition,
            markedSpringRowRanges
          )

    markNextSpringRows(springStatusCount, springRowRanges)

  def combinations(springStatusChunk: String) =
    val shiftToRightCount = springStatusChunk

    // *#??* => #*?*
    @tailrec
    def combinationsHelper(springStatus: Option[Char], restLeftStatus: String, result: String = ""): String =
      springStatus match
        case Some(status) =>
          if (status == SpringStatus.Malfunction.getValue()) {
            return combinationsHelper(restLeftStatus.headOption, restLeftStatus.tail, result :+ status)
          }

          val nextStatusOption = restLeftStatus.headOption
          nextStatusOption match {
            case Some(nextStatus)
                if nextStatus == SpringStatus.Malfunction.getValue() || nextStatus == SpringStatus.Marked.getValue() =>
              combinationsHelper(springStatus, restLeftStatus.tail, result :+ nextStatus)
            case Some(nextStatus) if nextStatus == SpringStatus.Unknown.getValue() =>
              combinationsHelper(springStatus, restLeftStatus.tail, result :+ nextStatus)
            case _ =>
              combinationsHelper(restLeftStatus.tail.headOption, restLeftStatus.tail.drop(1), result :+ status)
            // val skipStatus =
            //   restLeftStatus.tail.takeWhile(springStatus => springStatus != SpringStatus.Marked.getValue())
            // val newResult = (result :+ status) + skipStatus
            // val remainingStatus = restLeftStatus.tail.slice(skipStatus.length(), restLeftStatus.tail.length())
            // combinationsHelper(remainingStatus.headOption, remainingStatus.tail, newResult)
          }
        // if (nextStatus == SpringStatus.Marked.getValue()) {
        //   combinationsHelper(springStatus, restLeftStatus.tail, result :+ nextStatus)
        // }
        case _ =>
          result

    if (springStatusChunk.contains(SpringStatus.Unknown.getValueStr())) {
      combinationsHelper(springStatusChunk.headOption, springStatusChunk.tail)
    } else {
      springStatusChunk
    }

  def xx(springRowRange: SpringRowRange) =
    val springRanges = springRowRange.ranges
    val springStatusRowChunk = springRowRange.springStatusChunk
    val chunks = springRanges.reverse.map(springRange =>
      springStatusRowChunk.slice(springRange.range.start, springRange.range.end)
    )

    val tt = chunks.foreach(t => {
      println(f"--->${t}")
      println(combinations(t))
    })

  @tailrec
  def findMalfunctions(springRowStatus: String, ranges: Seq[Range] = Seq()): Seq[Range] = {
    val malfunctionIdx = springRowStatus.indexOf(SpringStatus.Malfunction.getValueStr())
    if (malfunctionIdx == -1) {
      ranges
    } else {
      val malfunctionCount = springRowStatus.drop(malfunctionIdx).count(_ == SpringStatus.Malfunction.getValue())
      val malfunctionRange = Range(malfunctionIdx, malfunctionIdx + malfunctionCount)
      findMalfunctions(
        springRowStatus.slice(malfunctionRange.end + 1, springRowStatus.length()),
        ranges :+ malfunctionRange
      )
    }
  }
// here
  def malfunctionRanges(springRowStatus: String): Seq[Range] = {

    @tailrec
    def malfunctionRangesHelper(
        springRowStatus: Seq[(Char, Int)],
        malfunctionChunk: Option[(Int, String)] = Option.empty,
        result: Seq[Range] = Seq()
    ): Seq[Range] = {
      if (springRowStatus.isEmpty) {
        return malfunctionChunk
          .map(chunk => {
            val malfunctionRange = Range(chunk._1, chunk._1 + chunk._2.length())
            result :+ malfunctionRange
          })
          .getOrElse(
            result
          )
      }

      val (springStatus, springIdx) = springRowStatus.head
      if (springStatus == SpringStatus.Malfunction.getValue()) {
        val newMalfunctionChunk = malfunctionChunk
          .map(chunk => (chunk._1, chunk._2 :+ springStatus))
          .getOrElse(
            (springIdx, springStatus.toString())
          )
        malfunctionRangesHelper(springRowStatus.tail, Some(newMalfunctionChunk), result)
      } else {
        malfunctionChunk match {
          case Some(malfunctionChunk) =>
            val malfunctionRange = Range(malfunctionChunk._1, malfunctionChunk._1 + malfunctionChunk._2.length())
            malfunctionRangesHelper(springRowStatus.tail, None, result :+ malfunctionRange)
          case _ =>
            malfunctionRangesHelper(springRowStatus.tail, None, result)
        }
      }
    }

    malfunctionRangesHelper(springRowStatus.zipWithIndex)
  }

  def bestMatch(springRow: SpringRow) = {
    println(malfunctionRanges(springRow.springsStatus))
  }

  @main def day12Main(): Unit =
    Using.Manager { use =>
      try {
        val springsData =
          use(Source.fromResource("day12/smallInput.txt")).getLines().toSeq
        val springRows = parseSpringData(springsData)
        springRows.foreach(bestMatch(_))
        // val markedSpringRowRanges = splitSpringRowIntoRanges(springRows)
        //   .map((springRowRangeTuple) => {
        //     val (springRow, springROwRanges) = springRowRangeTuple
        //     markSpringRowRanges(springRow.springsCount, springROwRanges)
        //   })
        //   .foreach(x => {
        //     println(x)
        //     x.map(xx)
        //   })

        // pringRowRanges.map(calculateCombinations))
      } catch {
        case e: RuntimeException => e.printStackTrace()
      }
    }

  // *?*??
  // [0,2],[2,4]
  // 0 + 1 not in [2,4] => 3
  // 1 + 1 is in [2,4] => new range [3,4] => 2
  // 2 + 1 is in [2,4] => new range [4,4] => 1

  // *?*???
  // [0,3], [2,5]
  // 0 + 1 not [2,5] => 4
  // 1 + 1 is [2,5] => new range [3,5] => 3
  // 2 + 1 is [2,5] => new range [4,5] => 2
  // 3 + 1 is [2,5] => new range [5,5] => 1

  // *??*??*??
  // [0, 4], [3,6], [6, 8]
  // 0 + 1 not in [3,6] => 4 + 3 = 7
  // 1 + 1 not in [3,6] => 4 + 3 = 7
  // 2 + 1 is [3,6] => new range [4,6] => 3 + 3 = 6
  // 3 + 1 is [3,6] => new range [5,6] => 2 + 3 = 5
  // 4 + 1 is [3,6] => new range [6,6] is in range [6,8] => [6,6][7,8] => 1 + 2 = 3

  // *?*#?? (1, 2)
  // [0,1], [2,4] => 1, 2
  // 0 + 1 not in [2,4] =>  2
  // 1 + 1 in [2,4] => new range [3,4] => 1

  // *?*##? (1, 3)
  // [0,1], [2,5] => 2, 2
  // 0 + 1 not in [2,5] => 2
  // 1 + 1 in [2,5] => new range [3,5] => 1
end Day12Challenge
