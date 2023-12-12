import scala.io.Source
import scala.util.Using

case class MapRange(destinationStart: Long, sourceStart: Long, length: Long) {
    def mapValue(value: Long): Option[Long] = {
        if (value >= sourceStart && value <= sourceStart + length - 1) {
            val diff = value - sourceStart
            return Some(destinationStart + diff)
        }

        None
    }
}

case class MapRanges(id: String, ranges: Seq[MapRange]) {
    def mapValue(value: Long): Option[Long] = 
        ranges.map(range => range.mapValue(value))
                .find( _.isDefined ) match {
                    case Some(mappedValue) => mappedValue
                    case _ => None
                }
}

class MapNode(private val mapRanges: MapRanges, private val nextNode: MapNode) {

    def mapValue(value: Long): Seq[Long] = {
        val mappedValue = mapRanges.mapValue(value).getOrElse(value)
        if (nextNode != null) {
            mappedValue +: nextNode.mapValue(mappedValue)
        } else {
            Seq(mappedValue)
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

    val nextNode = createMappingChain(mapRanges.tail)
    MapNode(mapRanges.head, nextNode)
} 

def processSeedsLocations(listOfMaps: Seq[String]): Seq[Seq[Long]] = {
    val seeds = parseSeeds(listOfMaps.head)
    val mapRanges = parseMappings(listOfMaps.tail)
    val mapNode = createMappingChain(mapRanges)

    seeds.map(seed => seed +: mapNode.mapValue(seed))
}

@main def day5Solution: Unit = {
    Using.Manager { use =>

        val listOfMaps = use(Source.fromResource("day5/input.txt")).getLines().toSeq
        val seedsMappings = processSeedsLocations(listOfMaps)

        // seedsMappings.foreach(seedMapping => println(seedMapping))
        
        val closestLocation = seedsMappings.map(seedMapping => seedMapping.reverse.head).min
        println(f"Closest seed location: ${closestLocation}")
    }
}