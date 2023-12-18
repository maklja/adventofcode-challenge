import scala.io.Source
import scala.util.Using

case class CardBid(card: String, bidAmount: Int, strengthValue: Int)

val cardValues = Seq[Char]('A', 'K', 'Q', 'T', '9', '8', '7', '6', '5', '4', '3', '2', 'J').zipWithIndex

val cardStrength = Seq[String]("5", "41", "32", "311", "221", "2111", "11111").zipWithIndex

def getCardValue(card: Char): Int = 
    cardValues.find(curCard => curCard._1 == card).map( _._2 ).getOrElse(0)

def compareCards(card1: String, card2: String): Boolean = {
    val card1Strength = getCardValue(card1.head)
    val card2Strength = getCardValue(card2.head)

    if (card1Strength == card2Strength) {
        compareCards(card1.tail, card2.tail)
    } else {
        card1Strength > card2Strength
    }
}

def compareCardBids(cardBid1: CardBid, cardBid2: CardBid) = {
    if (cardBid1.strengthValue == cardBid2.strengthValue) {
        compareCards(cardBid1.card, cardBid2.card)
    } else {
        cardBid1.strengthValue > cardBid2.strengthValue
    }
}

def getCardStrength(cardsStrength: String): Int = 
    cardStrength.find(curCardStrength => curCardStrength._1 == cardsStrength)
                .map( _._2 ).getOrElse(0)

def calcCardStrengthValue(card: String): Int = {
    val cardsStrengthMap = card.groupMapReduce( _.toString() )(_ => 1)(_ + _)
    val joker = cardsStrengthMap.getOrElse("J", 0)
    val maxCards = (cardsStrengthMap - "J")
        .reduceLeftOption((card1, card2) => if(card1._2 > card2._2) card1 else card2)
        .map(cards => (cards._1, cards._2 + joker))
        .getOrElse(("AAAAA", joker))
    val cardsStrength = (cardsStrengthMap - "J" + maxCards)
        .values
        .toSeq
        .sortBy(count => -count)
        .foldLeft("")((sum, curValue) => sum + curValue)
    getCardStrength(cardsStrength)  
}

def parseCardsData(cardData: Seq[String]) = 
    cardData.map(curCardBid => {
        val cardBid = curCardBid.split(" ")
        CardBid(cardBid(0), cardBid(1).toInt, calcCardStrengthValue(cardBid(0)))
    })
    .sortWith((cardBid1, cardBid2) => compareCardBids(cardBid1, cardBid2))

@main def day7Solution: Unit = {
    Using.Manager { use =>
        
        val cardData = use(Source.fromResource("day7/input.txt")).getLines().toSeq
        val totalWinnings = parseCardsData(cardData).zipWithIndex
            .foldLeft(0L)((sum, cardBidIdx) => sum + (cardBidIdx._2 + 1) * cardBidIdx._1.bidAmount)

        println(f"Total winnings: ${totalWinnings}")
    }
}