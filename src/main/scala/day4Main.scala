import scala.io.Source
import scala.util.Using

case class Card(id: Int, winningCardNumbers: Seq[Int], cardNumbers: Seq[Int])

def parseCardData(cardData: String) = {
    val trimmedCardData = cardData.trim.replaceAll(" +", " ")
    val s"Card $cardId: $winningCardNumbers | $cardNumbers" = trimmedCardData : @unchecked

    val winningCardNumbersSeq = winningCardNumbers.split(" ").map( _.toInt ).toSeq
    val cardNumbersSeq = cardNumbers.split(" ").map( _.toInt ).toSeq

    Card(cardId.toInt, winningCardNumbersSeq, cardNumbersSeq)
}

def processCardWinningNumbers(cardsData: Seq[String]) = 
    cardsData.map(cardData => {
        val card = parseCardData(cardData)
        val winMatch = card.cardNumbers.intersect(card.winningCardNumbers).length
        (card, winMatch)
    }).foldLeft(Map[Int, Int]())((map, cardResults) => {
        val cardsRange = Range.inclusive(cardResults._1.id + 1, cardResults._1.id + cardResults._2)
        val cardCount = map.get(cardResults._1.id).getOrElse(0) + 1
        cardsRange.foldLeft(map)((map, cardId) => 
            map.updatedWith(cardId)(curValue => curValue match {
                case Some(cardNum) => Some(cardNum + cardCount)
                case None => Some(cardCount)
            })
        ) + (cardResults._1.id -> cardCount)
    })

@main def day4Solution: Unit = {
    Using.Manager { use =>

        val engineSchema = use(Source.fromResource("day4/input.txt")).getLines().toSeq
        val totalCards = processCardWinningNumbers(engineSchema).values.sum

        println(f"Total cards sum: ${totalCards}")
    }
}