import scala.io.Source
import scala.util.Using

case class MapRange(destinationStart: Long, sourceStart: Long, length: Long) {
    def canMapValue(value: Long) = value >= sourceStart && value <= sourceStart + length - 1

    def mapValue(value: Long): Long = {
        destinationStart + value - sourceStart
    }
}

case class MapRanges(id: String, ranges: Seq[MapRange]) {
    def mapValue(value: Long): Option[Long] = 
        ranges.find( _.canMapValue(value) ).map(range => range.mapValue(value))
}

class MapNode(private val mapRanges: MapRanges, private val nextNode: MapNode) {

    def mapValue(value: Long): Long = {
        val mappedValue = mapRanges.mapValue(value).getOrElse(value)
        if (nextNode != null) {
            nextNode.mapValue(mappedValue)
        } else {
            mappedValue
        }
    }

    override def toString(): String = {
        if (nextNode == null) {
            return mapRanges.id
        }

        f"${mapRanges.id} -> ${nextNode}"
    }
}

def parseSeeds(seedsData: String) = {
    val s"seeds: $seeds" = seedsData : @unchecked
    seeds.split(" ").map( _.trim.toLong ).toSeq
}

def parseMapRanges(mapRanges: Seq[String]) =
    mapRanges.map(mapRange => {
        val Array(sourceStart, destinationStart, rangeLength) = mapRange.split(" ").map( _.trim.toLong )
        MapRange(sourceStart, destinationStart, rangeLength)
    })

def parseMappings(listOfMaps: Seq[String]): Seq[MapRanges] = {
    if (listOfMaps.isEmpty) {
        return Seq()
    }

    if (listOfMaps.head.isEmpty) {
        return parseMappings(listOfMaps.tail)
    }

    val mapName = listOfMaps.head.replace("map:", "").trim
    val mapRanges = parseMapRanges(listOfMaps.tail.takeWhile(line => !line.isEmpty))

    MapRanges(mapName, mapRanges) +: parseMappings(listOfMaps.tail.slice(mapRanges.length, listOfMaps.tail.length )) 
}

def createMappingChain(mapRanges: Seq[MapRanges]): MapNode = {
    if (mapRanges.length == 1) {
        return MapNode(mapRanges.head, null)
    }

    MapNode(mapRanges.head, createMappingChain(mapRanges.tail))
}

def minLocation(startSeed: Long, seedRange: Long, mapNode: MapNode) = {
    println(f"Seed ${startSeed}, ${seedRange}")
    var min: Long = Long.MaxValue
    for (i <- startSeed to startSeed + seedRange) {
        min = Math.min(min, mapNode.mapValue(i))
    }
    min
}

def processSeedsLocations(listOfMaps: Seq[String]): Long = {
    val seeds = parseSeeds(listOfMaps.head)
    val mapRanges = parseMappings(listOfMaps.tail)
    val mapNode = createMappingChain(mapRanges)

    seeds.sliding(2, 2).map(slidingPair => minLocation(slidingPair(0), slidingPair(1), mapNode))
        .reduce((min1, min2) => Math.min(min1, min2))
}

@main def day5Solution: Unit = {
    Using.Manager { use =>

        val listOfMaps = use(Source.fromResource("day5/smallInput.txt")).getLines().toSeq
        val closestLocation = processSeedsLocations(listOfMaps)

        println(f"Closest seed location: ${closestLocation}")
    }
}