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

  case class SpringRow(status: String, groups: Seq[Int])

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

    val newStatus = status
      .patch(
        startRange,
        SpringStatus.Malfunction.getValueStr().repeat(group),
        group
      )

    Some((newStatus, endRange))
  }

  def markContinuesGroup1(status: String, group: Int, position: Int = 0): Option[(String, Int)] = {
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
      status: String,
      groups: Seq[Int],
      position: Int = 0,
      markedStatus: Seq[Option[String]] = Seq()
  ): Seq[Option[String]] = {
    if (groups.isEmpty) {
      return markedStatus :+ Some(
        status.replace(SpringStatus.Unknown.getValueStr(), SpringStatus.Operational.getValueStr())
      )
    }

    val group = groups.head
    val skippedOperationalCount =
      status.slice(position, status.length()).takeWhile(_ == SpringStatus.Operational.getValue()).length()
    val groupRange = Range(position + skippedOperationalCount, status.length())
    groupRange.flatMap(curPosition => {
      val result = markContinuesGroup(status, group, curPosition)
      result match {
        case Some((newStatus, newPosition)) =>
          markGroups(newStatus, groups.tail, newPosition, markedStatus)
        case _ => None
      }
    })
  }

  def markGroupsSlim(
      springRow: SpringRow,
      remainingGroups: Seq[Int],
      position: Int = 0
  ): Option[SpringRow] = {
    if (remainingGroups.isEmpty) {
      // val cleanedStatus = springRow.status.dropWhile(_ == SpringStatus.Unknown.getValue())
      // val cleanedSpringRow = springRow.copy(status = cleanedStatus)
      return if (patternMatch1(springRow)) Some(springRow) else None
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
      val result = markContinuesGroup1(status, group, curPosition)
      result match {
        case Some((newStatus, newPosition)) =>
          val skipNextPosition =
            newStatus.lift(newPosition).map(_ == SpringStatus.Unknown.getValue()).getOrElse(false)
          val p = if (skipNextPosition) newPosition + 1 else newPosition
          markGroupsSlim(springRow.copy(status = newStatus), remainingGroups.tail, p)
            .orElse(findMatch(groupRange.tail))
        case _ => findMatch(groupRange.tail)
      }
    }

    findMatch(Range(position + skippedOperationalCount, status.length()))
  }

  def patternMatch1(springRow: SpringRow): Boolean = {
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

  def patternMatch(status: String, groups: Seq[Int]): Boolean = {
    val normalizedStatus = status
      .replaceAll("""(\.)+""", SpringStatus.Operational.getValueStr())
      .dropWhile(_ == SpringStatus.Operational.getValue())

    val statusParts = normalizedStatus.split("""(\.)""")
    if (statusParts.length != groups.length) {
      return false
    }

    groups.zip(statusParts).forall(tuple => tuple._1 == tuple._2.length())
  }

  def hotSpringCombinations(springRow: SpringRow) = {
    markGroups(springRow.status, springRow.groups)
      .filter(newStatus => {
        newStatus match {
          case Some(value) => patternMatch(value, springRow.groups)
          case _           => false
        }
      })
      .distinct
      .length
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
    val springRowPart = SpringRow(statusPart, groupsPart)

    val remainingStatusPart = status.drop(malfunctionBegin.length() + statusPart.length())
    val remainingGroups = groups.drop(statusPartCount)

    defineSpringRowParts(
      springRow.copy(status = remainingStatusPart, groups = remainingGroups),
      springRowParts :+ springRowPart
    )
  }

  def factorial(n: Int): Int = {
    (1 to n).foldLeft(1)(_ * _)
  }

  def calcSpringRowCombinations(springRow: SpringRow): Int = {
    println(springRow)
    val groupsCount = springRow.groups.length
    val totalSpots = springRow.status.length()
    val takenSpots = springRow.groups.map(_ - 1).sum
    val spacesBetweenMalfunctions = groupsCount - 1
    val remainingSpots = totalSpots - takenSpots - spacesBetweenMalfunctions

    factorial(remainingSpots) / (factorial(groupsCount) * factorial(remainingSpots - groupsCount))
  }

  def trackGroupRanges(springRow: SpringRow): Unit = {
    if (springRow.groups.isEmpty) {
      return;
    }

    val status = springRow.status
    val groups = springRow.groups
    val statusPart = status.takeWhile(_ != SpringStatus.Malfunction.getValue())

    // val statusPartCount =
    //   statusPart
    //     .replaceAll("""(\?)+""", SpringStatus.Unknown.getValueStr())
    //     .split(f"""\\${SpringStatus.Unknown.getValueStr()}""")
    //     .length
    // val groupsPart = groups.take(statusPartCount)
    // val springRowPart = SpringRow(statusPart, groupsPart)

    // val remainingStatusPart = status.drop(malfunctionBegin.length() + statusPart.length())
    // val remainingGroups = groups.drop(statusPartCount)

    // defineSpringRowParts(
    //   springRow.copy(status = remainingStatusPart, groups = remainingGroups),
    //   springRowParts :+ springRowPart
    // )
  }

  ///   // 525152
  // 400940731970390
  @main def day12Main(): Unit =
    Using.Manager { use =>
      try {
        val springsData =
          use(Source.fromResource("day12/smallInput.txt")).getLines().toSeq
        val springRows = parseSpringData(springsData)
        val hotSpringCombinationsSum = springRows.par.map(hotSpringCombinations).sum
        println(f"Hot springs combinations count: ${hotSpringCombinationsSum}")

        val hotSpringCombinationsSum1 =
          springRows
            .map(springRow => markGroupsSlim(springRow, springRow.groups))
            .flatten
            // .foreach(println(_))
            .map(springRow => defineSpringRowParts(springRow))
            .foreach(x => println(x))

        println(hotSpringCombinationsSum1)
        // val xx =
        //   springRows.map(springRow => SpringRow(f"${springRow.status}?", springRow.groups))
        // val xxx =
        //   springRows.map(springRow => SpringRow(f"?${springRow.status}", springRow.groups))
        // val hotSpringCombinationsSum1 = xx.map(hotSpringCombinations)
        // println(f"Hot springs combinations count: ${hotSpringCombinationsSum1}")
        // val hotSpringCombinationsSum2 = xxx.map(hotSpringCombinations)
        // println(f"Hot springs combinations count: ${hotSpringCombinationsSum2}")

      } catch {
        case e: RuntimeException => e.printStackTrace()
      }
    }
end Day12Challenge

// __?_#?? 2,2
// ?###?_?? 3,1
// ?_##?_?? 3,1

// 4! / 2! 2! = 6

// ##?##??
// ##??##?
// ##???##
// ?##?##?
// ?##??##
// ??##?##
