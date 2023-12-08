import scala.io.Source
import scala.util.Using

@main def day1: Unit = {
  Using.Manager { use =>
    val inputBufferedSource = use(Source.fromResource("input1.txt"))

    val sum = inputBufferedSource.getLines().map((line) => {
      val firstDigit = line.find(c => c.isDigit).map(c => c.toString)
      val lastDigit = line.reverse.find(c => c.isDigit).map(c => c.toString)

      (firstDigit.get + lastDigit.get).toInt
    }).sum

    print("\nResult is\n")
    print(sum)
    print("\n")
  }
}
  