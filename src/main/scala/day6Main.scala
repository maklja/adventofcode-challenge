import scala.io.Source
import scala.util.Using

case class RaceResult(time: Long, distance: Long)

def calcHoldButtonTime(bestRaceResult: RaceResult, winDistanceAmount: Int = 1) = {
    // equation (time - i) * i = distance => i^2 - time * i + distance
    // i = (-b +/- sqrt(b^2 - 4*c)) / 2 
    val b = bestRaceResult.time
    val c = bestRaceResult.distance + winDistanceAmount

    val part = Math.sqrt(b * b - 4 * c)
    val rangeMin = Math.ceil((b - part) / 2).asInstanceOf[Long]
    val rangeMax = Math.floor((b + part) / 2).asInstanceOf[Long]

    Range.Long.inclusive(rangeMin, rangeMax, 1)
} 

def parseRaces(races: Seq[String]) = {
    val raceData = races.map(race => race.trim.replaceAll(" +", " "))
    val timeData = raceData(0).replace("Time:", "").replaceAll(" +", "").toLong
    val distanceData = raceData(1).replace("Distance:", "").replaceAll(" +", "").toLong
    
    RaceResult(timeData, distanceData)
}

@main def day6Solution: Unit = {
    Using.Manager { use =>

        val raceTimes = use(Source.fromResource("day6/input.txt")).getLines().toSeq
        val raceResult = parseRaces(raceTimes)
        val variationsCount = calcHoldButtonTime(raceResult).length

        println(f"Races variations count: ${variationsCount}")
    }
}