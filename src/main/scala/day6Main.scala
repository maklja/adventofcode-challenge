import scala.io.Source
import scala.util.Using

case class RaceResult(time: Int, distance: Int)

def calcHoldButtonTime(bestRaceResult: RaceResult, winDistanceAmount: Int = 1) = {
    // equation (time - i) * i = distance => i^2 - time * i + distance
    val b = bestRaceResult.time 
    val c = bestRaceResult.distance + winDistanceAmount

    val part = Math.sqrt(b * b - 4 * c)
    val rangeMin = Math.ceil((b - part) / 2).asInstanceOf[Int]
    val rangeMax = Math.floor((b + part) / 2).asInstanceOf[Int]

    Range.Inclusive(rangeMin, rangeMax, 1)
} 

def parseRaces(races: Seq[String]) = {
    val raceData = races.map(race => race.trim.replaceAll(" +", " "))
    val timeData = raceData(0).replace("Time:", "").trim().split(" ").map( _.toInt ).toSeq
    val distanceData = raceData(1).replace("Distance:", "").trim().split(" ").map( _.toInt ).toSeq
    
    timeData.zipWithIndex.map((indexedTime) => RaceResult(indexedTime._1, distanceData(indexedTime._2)))
}

@main def day6Solution: Unit = {
    Using.Manager { use =>

        val raceTimes = use(Source.fromResource("day6/input.txt")).getLines().toSeq
        val variationsCount = parseRaces(raceTimes).map(raceResult => 
            calcHoldButtonTime(raceResult).length
        ).reduce((holdTime1, holdTime2) => holdTime1 * holdTime2)

        println(f"Races variations count: ${variationsCount}")
    }
}