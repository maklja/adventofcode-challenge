import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import Day12Challenge.SpringStatus.canBeMalfunction

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
  object SpringStatus:
    def canBeMalfunction(value: Char) =
      value == SpringStatus.Malfunction.getValue() || value == SpringStatus.Unknown.getValue()

    def isConcrete(value: Char) =
      value == SpringStatus.Malfunction.getValue() || value == SpringStatus.Operational.getValue()

  case class SpringRow(status: String, groups: Seq[Int])

  case class MalfunctionRange(group: Int, range: Option[Range])

  case class MalfunctionGroup(status: String, ranges: Seq[MalfunctionRange])

  def parseSpringData(springData: Seq[String]) =
    springData.map(curSpringData => {
      val springsAndCounts = curSpringData.split(" ")
      val springs = springsAndCounts(0)
      val count = springsAndCounts(1).split(",").map(_.toInt).toSeq

      SpringRow(springs, count)
    })

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
    findMalfunctionRanges(status.drop(newRange.end), newMalfunctions)
  }

  def bestMatchRange(status: String, groups: Seq[Int], malfunctionRange: Range, prevRange: Option[MalfunctionRange]) = {
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
        results: Seq[MalfunctionRange] = Seq()
    ): Seq[MalfunctionRange] = {
      if (springStatusCounts.isEmpty) {
        return results
      }

      val sprintStatusCount = springStatusCounts.head
      if (malfunctionChunk.length() > sprintStatusCount) {
        return checkBestMatch(springStatusCounts.tail, results :+ MalfunctionRange(sprintStatusCount, None))
      }

      val missingFields = sprintStatusCount - malfunctionChunk.length()
      val maxLeft = Math.min(leftSideFields, missingFields)
      val maxRight = Math.min(rightSideFields, missingFields)
      val n = malfunctionChunk.length() + maxLeft + maxRight
      if (n < sprintStatusCount) {
        return checkBestMatch(springStatusCounts.tail, results :+ MalfunctionRange(sprintStatusCount, None))
      }

      val previousRangeEnd =
        prevRange.flatMap(rangeMatch => rangeMatch.range.map(_.start + rangeMatch.group)).getOrElse(0)

      val rangeStart = Math.max(malfunctionRange.start - maxLeft, previousRangeEnd)
      val rangeEnd = malfunctionRange.end + maxRight

      return results :+ MalfunctionRange(sprintStatusCount, Some(Range(rangeStart, rangeEnd)))
    }

    checkBestMatch(groups)
  }

  def lastPartCheck(springRow: SpringRow, malfunctionRange: MalfunctionRange) = {
    val remainingSpringRow = springRow.status.drop(malfunctionRange.range.get.start + malfunctionRange.group)

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
      rangeMatches: Seq[MalfunctionRange] = Seq()
  ): (Boolean, Seq[MalfunctionRange]) = {
    if (malfunctionRanges.isEmpty) {
      return rangeMatches.lastOption
        .map(range => {
          val matched = lastPartCheck(springRow, range).isEmpty
          (matched, rangeMatches ++ springRow.groups.map(MalfunctionRange(_, None)))
        })
        .getOrElse((true, springRow.groups.map(MalfunctionRange(_, None))))
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
        rangeMatches :+ MalfunctionRange(springRow.groups.head, None)
      )
    }
  }

  def applyUnknownStatus(
      status: String,
      malfunctionRange: MalfunctionRange,
      position: Int
  ): (String, MalfunctionRange) = {
    val statusChunk =
      status.drop(position).indexOfSlice(SpringStatus.Unknown.getValueStr().repeat(malfunctionRange.group))
    val newStatus = status.patch(
      position + statusChunk,
      SpringStatus.Malfunction.getValueStr().repeat(malfunctionRange.group),
      malfunctionRange.group
    )
    val offset = position + statusChunk + malfunctionRange.group
    val lastConcreteStatusIdx =
      status.drop(offset).indexWhere(SpringStatus.isConcrete(_))
    val endRange = if (lastConcreteStatusIdx == -1) newStatus.length() else lastConcreteStatusIdx + offset

    (newStatus, MalfunctionRange(malfunctionRange.group, Some(Range(position + statusChunk, endRange))))
  }

  def applyStatusRange(status: String, malfunctionRanges: Seq[MalfunctionRange], position: Int = 0): Unit = {
    val malfunctionRange = malfunctionRanges.head

    if (malfunctionRange.range.isEmpty) {
      val (newStatus, newMalfunctionRange) = applyUnknownStatus(status, malfunctionRange, position)
      println(f"${status} - ${newStatus} - ${newMalfunctionRange}")
    } else {
      println(f"${status} - ${malfunctionRange}")
    }
    // val range = malfunctionRange.range.getOrElse(Range(position, status.length()))

    // println(status)
    // val rr = range.map(curRange => {
    //   val updatedStatus = status.patch(
    //     curRange,
    //     SpringStatus.Malfunction.getValueStr().repeat(malfunctionRange.group),
    //     malfunctionRange.group
    //   )

    //   val controlCheck =
    //     updatedStatus.slice(curRange, status.length()).takeWhile(_ == SpringStatus.Malfunction.getValue()).length()

    //   if (controlCheck == malfunctionRange.group) {
    //     Some(updatedStatus)
    //   } else {
    //     Option.empty
    //   }
    // })

    // println(rr)
    // if (malfunctionsCount >= malfunctionRange.group) {
    //   val updatedStatus = status.patc
    // }

  }

  // malfunctionRange.range match {
  //   case None => {

  //   }
  //   cas
  // }

  case class MarkStatus(unknownCount: Int = 0, malfunctionMark: Int = 0, latestPosition: Int = 0) {
    def matchGroup(group: Int) = {
      unknownCount + malfunctionMark == group
    }
  }

  def markContinuesGroup(status: String, group: Int, position: Int = 0): Unit = {
    val markStatus = status.zipWithIndex
      .slice(position, status.length())
      .foldLeft(MarkStatus())((markStatus, inxStatus) => {
        val (curStatus, idx) = inxStatus
        if (markStatus.matchGroup(group)) {
          markStatus
        } else if (curStatus == SpringStatus.Malfunction.getValue()) {
          markStatus.copy(malfunctionMark = markStatus.malfunctionMark + 1, latestPosition = idx)
        } else if (curStatus == SpringStatus.Unknown.getValue()) {
          markStatus.copy(unknownCount = markStatus.unknownCount + 1, latestPosition = idx)
        } else {
          markStatus.copy(unknownCount = 0, latestPosition = idx)
        }
      })

    val afterStatus =
      status.lift(position + markStatus.latestPosition + 1).getOrElse(SpringStatus.Operational.getValue())
    if (afterStatus == SpringStatus.Malfunction.getValue()) {
      return markContinuesGroup(status, group, position + 1)
    }

    val startRange = position + markStatus.latestPosition - group
    val endRange = position + markStatus.latestPosition

    val newStatus = status.patch(startRange, "#".repeat(group), group) // .patch(endRange + 1, ".", 1)
    println(newStatus)
  }

  def bestMatch(springRow: SpringRow) = {

    markContinuesGroup(springRow.status, springRow.groups.head)
    // val malfunctionRanges = findMalfunctionRanges(springRow.status)
    // val results = xxx(springRow, malfunctionRanges)

    // println(results)
    // applyStatusRange(springRow.status, results._2)
    // results._2.foldLeft(springRow.status)((status, range) => {
    //   range.range match {
    //     case None =>
    //   }
    // })

    // results
  }

  @main def day12Main(): Unit =
    Using.Manager { use =>
      try {
        val springsData =
          use(Source.fromResource("day12/smallInput.txt")).getLines().toSeq
        val springRows = parseSpringData(springsData)
        springRows.foreach(x => {
          bestMatch(x)
          // println(f"${x} => ${bestMatch(x)}")
        })
      } catch {
        case e: RuntimeException => e.printStackTrace()
      }
    }
end Day12Challenge
