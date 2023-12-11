import scala.io.Source
import scala.util.Using

case class Card(id: String, winningCardNumbers: Seq[Int], cardNumbers: Seq[Int])

def parseCardData(cardData: String) = {
    val trimmedCardData = cardData.trim.replaceAll(" +", " ")
    val s"Card $cardId: $winningCardNumbers | $cardNumbers" = trimmedCardData : @unchecked

    val winningCardNumbersSeq = winningCardNumbers.split(" ").map( _.toInt ).toSeq
    val cardNumbersSeq = cardNumbers.split(" ").map( _.toInt ).toSeq

    Card(cardId, winningCardNumbersSeq, cardNumbersSeq)
}

def processCardWinningNumbers(cardsData: Seq[String]) = {
    cardsData.map(cardData => {
        val card = parseCardData(cardData)
        val winMatch = card.cardNumbers.intersect(card.winningCardNumbers).length
        if (winMatch > 1) {
            val points = Range.inclusive(1, winMatch - 1)
            points.fold(1)((sum, i) => sum * 2)
        } else if (winMatch == 1) {
            1
        } else {
            0
        }
    })
}

@main def day4Solution: Unit = {
    Using.Manager { use =>

        val engineSchema = use(Source.fromResource("day4/input.txt")).getLines().toSeq
        val totalPoints = processCardWinningNumbers(engineSchema).sum

         println(f"Total points sum: ${totalPoints}")
    }
}