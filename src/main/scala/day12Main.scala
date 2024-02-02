import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParSeq

object Day12Challenge:

  enum SpringStatus(value: Char):
    case Operational extends SpringStatus('.')
    case Malfunction extends SpringStatus('#')
    case Unknown extends SpringStatus('?')

    def getValue() = value

    def getValueStr() =
      value match
        case '.' => """."""
        case '#' => "#"
        case '?' => """?"""

  case class SpringRow(status: String, groups: Seq[Int]) {
    def unwrap(n: Int = 1) = {
      Range(0, n).foldLeft(this)((springRow, i) =>
        springRow.copy(
          status = f"${springRow.status}${SpringStatus.Unknown.getValueStr()}${status}",
          groups = springRow.groups.concat(groups)
        )
      )
    }
  }

  def parseSpringData(springData: Seq[String]) =
    springData
      .takeWhile(!_.isEmpty())
      .map(curSpringData => {
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
      if (status == SpringStatus.Malfunction.getValue()) {
        copy(malfunctionMark = malfunctionMark + 1, latestPosition = position)
      } else if (status == SpringStatus.Unknown.getValue()) {
        copy(unknownCount = unknownCount + 1, latestPosition = position)
      } else {
        copy(unknownCount = 0, latestPosition = position, locked = malfunctionMark > 0)
      }
    }
  }

  def markGroup(status: Seq[(Char, Int)], group: Int, markStatus: MarkStatus = MarkStatus()): Option[MarkStatus] = {
    if (status.isEmpty) {
      return None
    }

    val (curStatus, idx) = status.head
    val newMarkStatus = markStatus.markStatus(curStatus, idx + 1)
    if (newMarkStatus.matchGroup(group)) {
      return Some(newMarkStatus)
    }

    if (newMarkStatus.locked) {
      return None
    }

    markGroup(status.tail, group, newMarkStatus)
  }

  def markContinuesGroup(status: String, group: Int, position: Int = 0): Option[(String, Int)] = {
    val markStatusWithNoPosition = markGroup(status.slice(position, status.length()).zipWithIndex, group)
    if (markStatusWithNoPosition.isEmpty) {
      return None
    }

    val markStatus =
      markStatusWithNoPosition.get.copy(latestPosition = position + markStatusWithNoPosition.get.latestPosition)

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
      .patch(endRange, SpringStatus.Operational.getValueStr(), 1)
    Some((newStatus, endRange))
  }

  def markGroups(
      status: String,
      groups: Seq[Int],
      position: Int = 0,
      markedStatus: ParSeq[Option[String]] = ParSeq()
  ): ParSeq[Option[String]] = {
    if (groups.isEmpty) {
      return markedStatus :+ Some(
        status.replace(SpringStatus.Unknown.getValueStr(), SpringStatus.Operational.getValueStr())
      )
    }

    val group = groups.head
    val skippedOperationalCount =
      status.drop(position).takeWhile(_ == SpringStatus.Operational.getValue()).length()
    // TODO reduce last element
    val groupRange = Range(position + skippedOperationalCount, status.length()).par
    groupRange.flatMap(curPosition => {
      val result = markContinuesGroup(status, group, curPosition)
      result match {
        case Some((newStatus, newPosition)) =>
          markGroups(newStatus, groups.tail, newPosition, markedStatus)
        case _ => None
      }
    })
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

  def hotSpringCombinations(springRow: SpringRow): Long = {
    val before = System.currentTimeMillis
    val comb = markGroups(springRow.status, springRow.groups)
      .filter(newStatus => {
        newStatus match {
          case Some(value) => patternMatch(value, springRow.groups)
          case _           => false
        }
      })
      .flatten
      .toSet
      .size
    val totalTime = System.currentTimeMillis - before
    println(f"${comb} = ${totalTime / 1000}")
    comb
  }

  @main def day12Main(): Unit =
    Using.Manager { use =>
      try {
        val springsData =
          use(Source.fromResource("day12/input.txt")).getLines().toSeq
        val springRows = parseSpringData(springsData)
        val hotSpringCombinationsSum = springRows.par.map(hotSpringCombinations).sum
        println(f"Hot springs combinations count: ${hotSpringCombinationsSum}")

      } catch {
        case e: RuntimeException => e.printStackTrace()
      }
    }
end Day12Challenge
