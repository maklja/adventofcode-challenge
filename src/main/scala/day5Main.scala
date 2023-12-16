import scala.io.Source
import scala.util.Using

case class MapRange(destinationStart: Long, sourceStart: Long, length: Long) {
    val destinationEnd = destinationStart + length

    def canMapValue(value: Long) = value >= sourceStart && value <= sourceStart + length - 1

    def mapValue(value: Long): (Long, Long) = {
        val destinationValue = destinationStart + value - sourceStart
        (destinationValue, destinationStart + length - destinationValue)
    }
}

case class MapRanges(id: String, ranges: Seq[MapRange]) {
    def apply(id: String, ranges: Seq[MapRange]): MapRanges =
        new MapRanges(id, ranges.sortBy( _.destinationStart ))

    def findClosestDestinationRange(destinationValue: Long) = 
        ranges.find(r => destinationValue < r.destinationEnd) match {
            case Some(closestDestValue) => closestDestValue.destinationEnd
            case _ => Long.MaxValue
        }

    def mapValue(value: Long): (Long, Long) = 
        ranges.find( _.canMapValue(value) )
                .map(range => range.mapValue(value)) match {
                    case Some(mapRangeValues) => mapRangeValues
                    case _ => (value, findClosestDestinationRange(value))
                }
}

class MapNode(private val mapRanges: MapRanges, private val nextNode: MapNode) {

    def mapValue(value: Long): (Long, Long) = {
        val mappedValue = mapRanges.mapValue(value)
        if (nextNode != null) {
            val nextMappedValue = nextNode.mapValue(mappedValue._1)
            (nextMappedValue._1, Math.min(nextMappedValue._2, mappedValue._2))
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
    val range = Range.Long(startSeed, startSeed + seedRange, 1).toIterator
    while (range.hasNext) {
        val seed = range.next();
        val mappedValue = mapNode.mapValue(seed)
        min = Math.min(min, mappedValue._1)
        var toSkip = mappedValue._2 - 1
        while(toSkip > 0 && range.hasNext) 
            range.next()
            toSkip -= 1
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

        val listOfMaps = use(Source.fromResource("day5/input.txt")).getLines().toSeq
        val closestLocation = processSeedsLocations(listOfMaps)

        println(f"Closest seed location: ${closestLocation}")
    }
}