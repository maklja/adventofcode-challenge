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

  type RangeMatch = (Int, Option[Range])

  def findMalfunctionRanges(springRowStatus: String): Seq[Range] = {

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

  def bestMatchRange(springsStatus: String, springsCount: Seq[Int], malfunctionRange: Range) = {
    val malfunctionChunk = springsStatus.slice(malfunctionRange.start, malfunctionRange.end)
    val leftSideFields =
      springsStatus
        .slice(0, malfunctionRange.start)
        .reverse
        .takeWhile(_ != SpringStatus.Operational.getValue())
        .length()
    val rightSideFields =
      springsStatus
        .slice(malfunctionRange.end, springsStatus.length())
        .takeWhile(_ != SpringStatus.Operational.getValue())
        .length()

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

      val rangeStart = malfunctionRange.start - maxLeft
      val rangeEnd = malfunctionRange.end + maxRight

      return results :+ (sprintStatusCount, Some(Range(rangeStart, rangeEnd)))
    }

    checkBestMatch(springsCount)
  }

  def lastPartCheck(springRow: SpringRow, lastRangeMatch: RangeMatch) = {
    val remainingSpringRow = springRow.springsStatus.drop(lastRangeMatch._2.get.start + lastRangeMatch._1)

    def xxxxx(springsStatus: String, sprintCount: Seq[Int]): Seq[Int] = {
      if (springsStatus.isEmpty() || sprintCount.isEmpty) {
        return sprintCount
      }

      val operationalFields = springsStatus.takeWhile(_ == SpringStatus.Operational.getValue())
      val springStatus =
        springsStatus.drop(operationalFields.length()).takeWhile(_ != SpringStatus.Operational.getValue())

      val fieldsRequired = if (operationalFields.isEmpty()) sprintCount.head + 1 else sprintCount.head
      if (fieldsRequired <= springStatus.length()) {
        xxxxx(springsStatus.drop(operationalFields.length() + fieldsRequired), sprintCount.tail)
      } else {
        xxxxx(springsStatus.drop(operationalFields.length() + springStatus.length()), sprintCount)
      }
      // ##?.?.????? 3, 2, 1
    }

    xxxxx(remainingSpringRow, springRow.springsCount)
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
          (matched, rangeMatches ++ springRow.springsCount.map((_, None)))
        })
        .getOrElse((true, springRow.springsCount.map((_, None))))
    }

    val malfunctionRange = malfunctionRanges.head
    val lastRange = rangeMatches.lastOption.flatMap(_._2)
    val isTaken =
      lastRange.map(!_.intersect(malfunctionRange).isEmpty).getOrElse(false)

    if (isTaken) {
      return xxx(springRow, malfunctionRanges.tail, rangeMatches)
    }

    val bestRangeMatch =
      bestMatchRange(springRow.springsStatus, springRow.springsCount, malfunctionRange)

    val result = xxx(
      springRow.copy(springsCount = springRow.springsCount.drop(bestRangeMatch.length)),
      malfunctionRanges.tail,
      rangeMatches ++ bestRangeMatch
    )

    if (result._1) {
      result
    } else {
      xxx(
        springRow.copy(springsCount = springRow.springsCount.drop(1)),
        malfunctionRanges,
        rangeMatches :+ (springRow.springsCount.head, None)
      )
    }
  }

  def bestMatch(springRow: SpringRow): (Boolean, Seq[RangeMatch]) = {
    val malfunctionRanges = findMalfunctionRanges(springRow.springsStatus)
    val results = xxx(springRow, malfunctionRanges)

    results
  }

  @main def day12Main(): Unit =
    Using.Manager { use =>
      try {
        val springsData =
          use(Source.fromResource("day12/input.txt")).getLines().toSeq
        val springRows = parseSpringData(springsData)
        springRows.foreach(x => {
          println(f"${x} => ${bestMatch(x)}")
        })
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
end Day12Challenge
