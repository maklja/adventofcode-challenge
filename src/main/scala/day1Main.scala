import scala.io.Source
import scala.util.Using

object Day1Challenge:

  case class NumberPointer(name: String, value: Int, index: Int)

  val numbers: Seq[String] = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

  def joinFirstAndLastDigit(str: String): Int =
    val firstDigit = str.find(c => c.isDigit).map(c => c.toString)
    val lastDigit = str.reverse.find(c => c.isDigit).map(c => c.toString)

    (firstDigit.get + lastDigit.get).toInt

  def convertFirstAndLastStringToNumber(str: String): String =
    val indexedNumbers = numbers.zipWithIndex.map(n => (n._1, n._2 + 1))
    val firstPart = indexedNumbers
      .map(n => NumberPointer(n._1, n._2, str.indexOf(n._1)))
      .filter(_.index != -1)
      .minByOption(_.index)
      .map(p => str.patch(p.index, p.value.toString, 0))
      .getOrElse(str)

    indexedNumbers
      .map(n => NumberPointer(n._1, n._2, firstPart.lastIndexOf(n._1)))
      .filter(_.index != -1)
      .maxByOption(_.index)
      .map(p => firstPart.patch(p.index, p.value.toString, 0))
      .getOrElse(firstPart)

  @main def day1Solution: Unit =
    Using.Manager { use =>

      val calibrationData = use(Source.fromResource("day1/input.txt")).getLines().toSeq
      val calibrationValuesSum = calibrationData
        .map(joinFirstAndLastDigit)
        .sum
      println(f"Calibration value sum: ${calibrationValuesSum}")

      val properCalibrationValuesSum = calibrationData
        .map(calibrationValue => {
          val parsedCalValue = convertFirstAndLastStringToNumber(calibrationValue)
          joinFirstAndLastDigit(parsedCalValue)
        })
        .sum
      println(f"Proper calibration value sum: ${properCalibrationValuesSum}")
    }

end Day1Challenge
