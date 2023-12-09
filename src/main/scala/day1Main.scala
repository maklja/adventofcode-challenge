import scala.io.Source
import scala.util.Using

case class NumberPointer(name: String, value: Int, index: Int)

val numbers: Seq[String] = Seq("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")

def joinFirstAndLastDigit(str: String) = {
    val firstDigit = str.find(c => c.isDigit).map(c => c.toString)
    val lastDigit = str.reverse.find(c => c.isDigit).map(c => c.toString)

    (firstDigit.get + lastDigit.get).toInt
}

def convertFirstAndLastStringToNumber(str: String) = {
    val indexedNumbers = numbers.zipWithIndex.map(n => (n._1, n._2 + 1))
    val firstPart = indexedNumbers.map(n => NumberPointer(n._1, n._2, str.indexOf(n._1)))
                                  .filter { _.index != -1 }
                                  .minByOption { _.index }
                                  .map(p => str.patch(p.index, p.value.toString, 0))
                                  .getOrElse { str }

    val targetPart = indexedNumbers.map(n => NumberPointer(n._1, n._2, firstPart.lastIndexOf(n._1)))
                                  .filter { _.index != -1 }
                                  .maxByOption { _.index }
                                  .map(p => firstPart.patch(p.index, p.value.toString, 0))
                                  .getOrElse { firstPart }

    joinFirstAndLastDigit(targetPart)
}

@main def day1Solution: Unit = {
  Using.Manager { use =>

    val inputBufferedSource = use(Source.fromResource("day1/input.txt"))
    val sum = inputBufferedSource.getLines()
      .map(convertFirstAndLastStringToNumber)
      .sum

    print("\nResult is\n")
    print(sum)
    print("\n")
  }
}
  