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

  case class SpringRow(status: String, groups: Seq[Int])

  def parseSpringData(springData: Seq[String]) =
    springData.map(curSpringData => {
      val springsAndCounts = curSpringData.split(" ")
      val springs = springsAndCounts(0)
      val count = springsAndCounts(1).split(",").map(_.toInt).toSeq

      SpringRow(springs, count)
    })

  type RangeMatch = (Int, Option[Range])

  @tailrec
  def findMalfunctionRanges(status: String, malfunctions: Seq[Range] = Seq()): Seq[Range] = {
    val malfunctionIdx = status.indexOf(SpringStatus.Malfunction.getValueStr())
    if (malfunctionIdx == -1) {
      return malfunctions
    }

    val malfunctionSize =
      status.drop(malfunctionIdx).takeWhile(curStatus => curStatus == SpringStatus.Malfunction.getValue()).length()
    val rangeStart = malfunctions.lastOption.map(_.end).getOrElse(0)

    val newRange = Range(rangeStart + malfunctionIdx, rangeStart + malfunctionIdx + malfunctionSize)
    val newMalfunctions = malfunctions :+ newRange
    findMalfunctionRanges(status.drop(newRange.length), newMalfunctions)
  }

  def bestMatchRange(status: String, groups: Seq[Int], malfunctionRange: Range, prevRange: Option[RangeMatch]) = {
    val malfunctionChunk = status.slice(malfunctionRange.start, malfunctionRange.end)
    val leftSideFields =
      status
        .slice(0, malfunctionRange.start)
        .reverse
        .takeWhile(_ != SpringStatus.Operational.getValue())
        .length()
    val rightSideFields =
      status
        .slice(malfunctionRange.end, status.length())
        .takeWhile(_ != SpringStatus.Operational.getValue())
        .length()

    @tailrec
    def checkBestMatch(
        springStatusCounts: Seq[Int],
        results: Seq[RangeMatch] = Seq()
    ): Seq[RangeMatch] = {
      if (springStatusCounts.isEmpty) {
        return results
      }

      val sprintStatusCount = springStatusCounts.head
      if (malfunctionChunk.length() > sprintStatusCount) {
        return checkBestMatch(springStatusCounts.tail, results :+ (sprintStatusCount, None))
      }

      val missingFields = sprintStatusCount - malfunctionChunk.length()
      val maxLeft = Math.min(leftSideFields, missingFields)
      val maxRight = Math.min(rightSideFields, missingFields)
      val n = malfunctionChunk.length() + maxLeft + maxRight
      if (n < sprintStatusCount) {
        return checkBestMatch(springStatusCounts.tail, results :+ (sprintStatusCount, None))
      }

      val previousRangeEnd =
        prevRange.flatMap(rangeMatch => rangeMatch._2.map(_.start + rangeMatch._1)).getOrElse(0)

      val rangeStart = Math.max(malfunctionRange.start - maxLeft, previousRangeEnd)
      val rangeEnd = malfunctionRange.end + maxRight

      return results :+ (sprintStatusCount, Some(Range(rangeStart, rangeEnd)))
    }

    checkBestMatch(groups)
  }

  def lastPartCheck(springRow: SpringRow, lastRangeMatch: RangeMatch) = {
    val remainingSpringRow = springRow.status.drop(lastRangeMatch._2.get.start + lastRangeMatch._1)

    def xxxxx(status: String, sprintCount: Seq[Int]): Seq[Int] = {
      if (status.isEmpty() || sprintCount.isEmpty) {
        return sprintCount
      }

      val operationalFields = status.takeWhile(_ == SpringStatus.Operational.getValue())
      val springStatus =
        status.drop(operationalFields.length()).takeWhile(_ != SpringStatus.Operational.getValue())

      val fieldsRequired = if (operationalFields.isEmpty()) sprintCount.head + 1 else sprintCount.head
      if (fieldsRequired <= springStatus.length()) {
        xxxxx(status.drop(operationalFields.length() + fieldsRequired), sprintCount.tail)
      } else {
        xxxxx(status.drop(operationalFields.length() + springStatus.length()), sprintCount)
      }
    }

    xxxxx(remainingSpringRow, springRow.groups)
  }

  def xxx(
      springRow: SpringRow,
      malfunctionRanges: Seq[Range],
      rangeMatches: Seq[RangeMatch] = Seq()
  ): (Boolean, Seq[RangeMatch]) = {
    if (malfunctionRanges.isEmpty) {
      return rangeMatches.lastOption
        .map(range => {
          val matched = lastPartCheck(springRow, range).isEmpty
          (matched, rangeMatches ++ springRow.groups.map((_, None)))
        })
        .getOrElse((true, springRow.groups.map((_, None))))
    }

    val malfunctionRange = malfunctionRanges.head
    val lastRange = rangeMatches.lastOption.flatMap(_._2)
    val isTaken =
      lastRange.map(!_.intersect(malfunctionRange).isEmpty).getOrElse(false)

    if (isTaken) {
      return xxx(springRow, malfunctionRanges.tail, rangeMatches)
    }

    val bestRangeMatch =
      bestMatchRange(springRow.status, springRow.groups, malfunctionRange, rangeMatches.lastOption)

    val result = xxx(
      springRow.copy(groups = springRow.groups.drop(bestRangeMatch.length)),
      malfunctionRanges.tail,
      rangeMatches ++ bestRangeMatch
    )

    if (result._1) {
      result
    } else {
      xxx(
        springRow.copy(groups = springRow.groups.drop(1)),
        malfunctionRanges,
        rangeMatches :+ (springRow.groups.head, None)
      )
    }
  }

  def bestMatch(springRow: SpringRow): (Boolean, Seq[RangeMatch]) = {
    val malfunctionRanges = findMalfunctionRanges(springRow.status)
    val results = xxx(springRow, malfunctionRanges)

    results
  }

  @main def day12Main(): Unit =
    Using.Manager { use =>
      try {
        val springsData =
          use(Source.fromResource("day12/smallInput.txt")).getLines().toSeq
        val springRows = parseSpringData(springsData)
        springRows.foreach(x => {
          println(f"${x} => ${bestMatch(x)}")
        })
      } catch {
        case e: RuntimeException => e.printStackTrace()
      }
    }
end Day12Challenge
