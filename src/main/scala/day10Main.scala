import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

val north = 'n'
val south = 's'
val west = 'w'
val east = 'e'

val northSouth = '|'
val westEast = '-'
val northEast = 'L'
val northWest = 'J'
val southWest = '7'
val southEast = 'F'
val groundSymbol = '.'
val startPositionSymbol = 'S'

val northMap = Map(
    (northSouth -> north),
    (southEast -> east),
    (southWest -> west)
)

val southMap = Map(
    (northSouth -> south),
    (northWest -> west),
    (northEast -> east)
)

val westMap = Map(
    (westEast -> west),
    (northEast -> north),
    (southEast -> south)
)

val eastMap = Map(
    (westEast -> east),
    (northWest -> north),
    (southWest -> south)
)

val directionsMap = Map(
    (north -> northMap),
    (south -> southMap),
    (west -> westMap),
    (east -> eastMap),
)

case class PipeMap(map: Seq[String]) {

    def getSymbol(p: (Int, Int)): Char = map(p._1)(p._2)

    def positionFromDirection(p: (Int, Int), direction: Char): Option[(Int, Int)] =
        direction match
            case `north` => Some((p._1 - 1, p._2))
            case `south` => Some((p._1 + 1, p._2))
            case `west` => Some((p._1, p._2 - 1))
            case `east` => Some((p._1, p._2 + 1))
            case _ => None

    def findStartPosition(): Option[(Int, Int)] = 
        map.zipWithIndex.find(mapRow => mapRow._1.contains(startPositionSymbol)) match {
            case Some(mapRow) => Some((mapRow._2, mapRow._1.indexOf(startPositionSymbol)))
            case _ => None
        }
}

@tailrec
def searchPipeLine(positions: Seq[(Int, Int)], directions: Seq[Char], map: PipeMap): Option[Seq[(Int, Int)]] = {
    if (directions.isEmpty) {
        return None
    }

    val curDirection = directions.head
    val curPosition = positions.last
    val curSymbol = map.getSymbol(curPosition)
    val allowedNextSymbols = directionsMap(curDirection)
    val path = map.positionFromDirection(curPosition, curDirection) match {
        case Some(nextPosition) => {
            val nextSymbol = map.getSymbol(nextPosition)
            if (nextSymbol == startPositionSymbol) {
                Some(positions :+ nextPosition)
            } else if (allowedNextSymbols.keySet.contains(nextSymbol))  {
                val allowedNextDirection = allowedNextSymbols(nextSymbol)
                searchPipeLine(positions :+ nextPosition, Seq(allowedNextDirection), map)
            } else {
                searchPipeLine(positions, directions.tail, map)
            }
        }
        case _ => searchPipeLine(positions, directions.tail, map)
    }

    if (path.isDefined) {
        path
    } else {
        searchPipeLine(positions, directions.tail, map)
    }
}

def processPipeline(pipeData: Seq[String]) = {
    val map = new PipeMap(pipeData)
    val startPosition = map.findStartPosition() match {
        case Some(position) => position
        case _ => throw new RuntimeException("Starting position not found")
    }

    val path = searchPipeLine(Seq(startPosition), Seq(north, south, west,  east), map)
    println(path)
}

@main def day10Solution: Unit = {
    Using.Manager { use =>
        
        val pipeData = use(Source.fromResource("day10/smallInput.txt")).getLines().toSeq
        processPipeline(pipeData)
    }
}