import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.parallel.CollectionConverters._

object Day12Challenge:

  enum SpringStatus(value: Char):
    case Operational extends SpringStatus('.')
    case Malfunction extends SpringStatus('#')
    case Unknown extends SpringStatus('?')
    case Marked extends SpringStatus('_')

    def getValue() = value

    def getValueStr() =
      value match
        case '.' => """."""
        case '#' => "#"
        case '?' => """?"""
        case '_' => "_"

  val concreteSpringStatus = Seq(SpringStatus.Malfunction.getValue(), SpringStatus.Marked.getValue())

  case class SpringRow(status: String, groups: Seq[Int])

  case class GroupRange(groupStatus: String, range: Range, otherGroups: Seq[Int])

  def parseSpringData(springData: Seq[String]) =
    springData.map(curSpringData => {
      val springsAndCounts = curSpringData.split(" ")
      val springs = springsAndCounts(0)
      val count = springsAndCounts(1).split(",").map(_.toInt).toSeq

      SpringRow(springs, count)
    })

  case class MarkStatus(
      unknownCount: Int = 0,
      malfunctionMark: Int = 0,
      latestPosition: Int = 0,
      locked: Boolean = false
  ) {
    def matchGroup(group: Int) = {
      unknownCount + malfunctionMark == group
    }

    def markStatus(status: Char, position: Int) = {
      if (locked) {
        this
      } else if (status == SpringStatus.Malfunction.getValue()) {
        copy(malfunctionMark = malfunctionMark + 1, latestPosition = position)
      } else if (status == SpringStatus.Unknown.getValue()) {
        copy(unknownCount = unknownCount + 1, latestPosition = position)
      } else {
        copy(unknownCount = 0, latestPosition = position, locked = malfunctionMark > 0)
      }
    }
  }

  def markContinuesGroup(status: String, group: Int, position: Int = 0): Option[(String, Int)] = {
    val markStatus = status
      .slice(position, status.length())
      .zipWithIndex
      .foldLeft(MarkStatus())((markStatus, inxStatus) => {
        val (curStatus, idx) = inxStatus
        if (markStatus.matchGroup(group)) {
          markStatus
        } else {
          markStatus.markStatus(curStatus, position + idx + 1)
        }
      })

    if (!markStatus.matchGroup(group)) {
      return None
    }

    val afterStatus =
      status.lift(markStatus.latestPosition).getOrElse(SpringStatus.Operational.getValue())
    if (afterStatus == SpringStatus.Malfunction.getValue()) {
      return markContinuesGroup(status, group, position + 1)
    }

    val startRange = markStatus.latestPosition - group
    val endRange = markStatus.latestPosition

    val statusChunk = status
      .slice(startRange, startRange + group)
      .map(curStatus => {
        if (curStatus != SpringStatus.Unknown.getValue()) {
          curStatus
        } else {
          SpringStatus.Marked.getValue()
        }
      })
    val newStatus = status
      .patch(
        startRange,
        statusChunk,
        group
      )

    Some((newStatus, endRange))
  }

  def markGroups(
      springRow: SpringRow,
      remainingGroups: Seq[Int],
      position: Int = 0
  ): Option[SpringRow] = {
    if (remainingGroups.isEmpty) {
      return if (patternMatch(springRow)) Some(springRow) else None
    }

    val status = springRow.status
    val group = remainingGroups.head
    val skippedOperationalCount =
      status
        .slice(position, status.length())
        .takeWhile(curStatus => curStatus == SpringStatus.Operational.getValue())
        .length()

    def findMatch(groupRange: Range): Option[SpringRow] = {
      if (groupRange.isEmpty) {
        return None
      }
      val curPosition = groupRange.head
      val result = markContinuesGroup(status, group, curPosition)
      result match {
        case Some((newStatus, newPosition)) =>
          val skipNextPosition =
            newStatus.lift(newPosition).map(_ == SpringStatus.Unknown.getValue()).getOrElse(false)
          val p = if (skipNextPosition) newPosition + 1 else newPosition
          markGroups(springRow.copy(status = newStatus), remainingGroups.tail, p)
            .orElse(findMatch(groupRange.tail))
        case _ => findMatch(groupRange.tail)
      }
    }

    findMatch(Range(position + skippedOperationalCount, status.length()))
  }

  def patternMatch(springRow: SpringRow): Boolean = {
    val normalizedStatus = springRow.status
      .replace(SpringStatus.Marked.getValueStr(), SpringStatus.Malfunction.getValueStr())
      .replace(SpringStatus.Unknown.getValueStr(), SpringStatus.Operational.getValueStr())
      .replaceAll("""(\.)+""", SpringStatus.Operational.getValueStr())
      .dropWhile(_ == SpringStatus.Operational.getValue())

    val statusParts = normalizedStatus.split("""(\.)""")
    if (statusParts.length != springRow.groups.length) {
      return false
    }

    springRow.groups.zip(statusParts).forall(tuple => tuple._1 == tuple._2.length())
  }

  def defineSpringRowParts(springRow: SpringRow, springRowParts: Seq[SpringRow] = Seq()): Seq[SpringRow] = {
    if (springRow.groups.isEmpty) {
      return springRowParts;
    }

    val status = springRow.status
    val groups = springRow.groups
    val malfunctionBegin = status.takeWhile(curStatus => curStatus == SpringStatus.Operational.getValue())
    val statusPart =
      status.drop(malfunctionBegin.length()).takeWhile(_ != SpringStatus.Operational.getValue())

    val statusPartCount =
      statusPart
        .replaceAll("""(\?)+""", SpringStatus.Unknown.getValueStr())
        .split(f"""\\${SpringStatus.Unknown.getValueStr()}""")
        .length
    val groupsPart = groups.take(statusPartCount)

    val remainingStatusPart = status.drop(malfunctionBegin.length() + statusPart.length())
    val remainingGroups = groups.drop(statusPartCount)

    if (groupsPart.isEmpty) {
      val prevSpringRowPart = springRowParts.lastOption
      prevSpringRowPart match
        case Some(springRowPart) => {
          val updatedPrevSpringRowPart = springRowPart.copy(status = springRowPart.status + statusPart)
          defineSpringRowParts(
            springRow.copy(status = remainingStatusPart, groups = remainingGroups),
            springRowParts.dropRight(1) :+ updatedPrevSpringRowPart
          )
        }
        case _ => {
          defineSpringRowParts(
            springRow.copy(status = remainingStatusPart, groups = remainingGroups),
            springRowParts
          )
        }
    } else {
      val springRowPart = SpringRow(statusPart, groupsPart)
      defineSpringRowParts(
        springRow.copy(status = remainingStatusPart, groups = remainingGroups),
        springRowParts :+ springRowPart
      )
    }
  }

  def factorial(n: Int): Int = {
    (1 to n).foldLeft(1)(_ * _)
  }

  def groupStatusRange(groupStatus: String): Int = {
    val firstMalfunction = groupStatus.indexOf(SpringStatus.Malfunction.getValueStr())
    if (firstMalfunction == -1) {
      return firstMalfunction
    }
    val lastMalfunction = groupStatus.lastIndexOf(SpringStatus.Malfunction.getValueStr())
    val rightMarkedCount = groupStatus.reverse.takeWhile(_ == SpringStatus.Marked.getValue()).length()
    val group = groupStatus.dropRight(rightMarkedCount).length()

    group - (lastMalfunction - firstMalfunction + 1)
  }

  def scanStatusForGroup(status: String, groups: Seq[Int], position: Int = 0): Seq[GroupRange] = {
    val group = groups.head
    val indexStatus = status.zipWithIndex
    val groupStatus = indexStatus
      .drop(position)
      .foldLeft(("", -1))((groupStatus, currentIdxStatus) => {
        val (curStatus, idx) = currentIdxStatus
        if (groupStatus._1.length() == group) {
          groupStatus
        } else if (concreteSpringStatus.contains(curStatus)) {
          (groupStatus._1 + curStatus, idx + 1)
        } else {
          ("", -1)
        }
      })

    val groupOffset = groupStatusRange(groupStatus._1)
    val remainingStatusCount = indexStatus.last._2 + 1
    val minRange =
      if (groupOffset > -1)
        Math.min(groupStatus._2 + groupOffset, remainingStatusCount)
      else if (remainingStatusCount > groupStatus._2)
        remainingStatusCount
      else
        groupStatus._2

    if (groups.tail.isEmpty) {
      val groupRange = GroupRange(groupStatus._1, Range(groupStatus._2 - groupStatus._1.length(), minRange), Seq())
      Seq(groupRange)
    } else {
      val curGroupRanges = scanStatusForGroup(status, groups.tail, groupStatus._2 + 1);
      val nextRange = curGroupRanges.head
      val nextRangeMax = nextRange.range.end - nextRange.groupStatus.length() - 1
      val endRange = Math.min(minRange, nextRangeMax)
      val groupRange =
        GroupRange(groupStatus._1, Range(groupStatus._2 - groupStatus._1.length(), endRange), groups.tail)
      groupRange +: curGroupRanges
    }
  }

  def tt(groupRanges: Seq[GroupRange]) = {
    val groupsCount = groupRanges.length
    val spots = groupRanges.last.range.end - groupRanges.head.range.start
    val normalizedSpots = spots - groupRanges
      .map(_.groupStatus.length() - 1)
      .sum
    val normalizedSpotsNoSpaces = normalizedSpots - (groupsCount - 1)
    val status = groupRanges.map(_.groupStatus).mkString
    val totalSpotsCombinations = factorial(normalizedSpotsNoSpaces)
    val groupsCombinations = factorial(groupsCount) * factorial(normalizedSpotsNoSpaces - groupsCount)
    println(
      f"${status} = ${spots} = ${normalizedSpotsNoSpaces} = ${totalSpotsCombinations / groupsCombinations}"
    )
    println()
    totalSpotsCombinations / groupsCombinations
  }

  ///   // 525152
  // 400940731970390
  @main def day12Main(): Unit =
    Using.Manager { use =>
      try {
        val springsData =
          use(Source.fromResource("day12/smallInput.txt")).getLines().toSeq
        val springRows = parseSpringData(springsData)

        val hotSpringCombinationsSum1 =
          springRows
            .map(springRow => markGroups(springRow, springRow.groups))
            .flatten
            .map(springRow => {
              val springRowParts = defineSpringRowParts(springRow)
              val combinations = springRowParts
                .map(springRowPart => {
                  val groupRanges = scanStatusForGroup(springRowPart.status, springRowPart.groups)
                  val xx = groupRanges.foldLeft(Seq[Seq[GroupRange]]())((intersections, groupRange) => {
                    val lastRanges = intersections.lastOption.getOrElse(Seq())
                    val lastRange = lastRanges.lastOption
                    lastRange match
                      case Some(lastGroupRange) if (!lastGroupRange.range.intersect(groupRange.range).isEmpty) =>
                        intersections.dropRight(1) :+ (lastRanges :+ groupRange)
                      case _ => intersections :+ (Seq() :+ groupRange)
                  })
                  xx.map(tt).fold(1)(_ * _)
                })
              // .tapEach(x => println(x))
              // .zipWithIndex
              // .map(idxSpringGroupRange => {
              //   val (springGroupRange, idx) = idxSpringGroupRange
              //   springGroupRange.combinations()
              // })
              println(combinations)
              combinations.fold(1)(_ * _)
            })
            .sum

        println(hotSpringCombinationsSum1)
      } catch {
        case e: RuntimeException => e.printStackTrace()
      }
    }
end Day12Challenge

// ?#??? (2,1)
// ##?#?
// ##?#?
// ?##?#

// __?_#?? 2,2
// ?###?_?? 3,1
// ?_##?_?? 3,1

// 4! / 2! 2! = 6

//?###?#??
//?###??#?
//?###???#

// ##?##??
// ##??##?
// ##???##
// ?##?##?
// ?##??##
// ??##?##

// ?#????? (3,1)

// ###?#??
// ###??#?
// ###???#
// ?###?#?
// ?###??#

// 1 hardcoded / 3 expected => +/- 2
// allowed combinations 2
// 3 - 2 = 1

// 7 => 4
// 4! / 2! * 2! => 6 - 1

// ???#??#?? (2,2)
// ??##?##??
// ??##??##?
// ???#??##?

// 7 - 2 - 1 = 4
// 4! / 2! *2! => 6

// 4 - 2 = 2
// 3 - 2 = 1
