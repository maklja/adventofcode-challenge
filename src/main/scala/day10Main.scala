import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

val north = 'n'
val south = 's'
val west = 'w'
val east = 'e'

val northSouthSymbol = '|'
val westEastSymbol = '-'
val northEastSymbol = 'L'
val northWestSymbol = 'J'
val southWestSymbol = '7'
val southEastSymbol = 'F'
val groundSymbol = '.'
val startPositionSymbol = 'S'

val northMap = Map(
    (northSouthSymbol -> north),
    (southEastSymbol -> east),
    (southWestSymbol -> west),
    (startPositionSymbol -> startPositionSymbol)
)

val southMap = Map(
    (northSouthSymbol -> south),
    (northWestSymbol -> west),
    (northEastSymbol -> east),
    (startPositionSymbol -> startPositionSymbol)
)

val westMap = Map(
    (westEastSymbol -> west),
    (northEastSymbol -> north),
    (southEastSymbol -> south),
    (startPositionSymbol -> startPositionSymbol)
)

val eastMap = Map(
    (westEastSymbol -> east),
    (northWestSymbol -> north),
    (southWestSymbol -> south),
    (startPositionSymbol -> startPositionSymbol)
)

val directionsMap = Map(
    (north -> northMap),
    (south -> southMap),
    (west -> westMap),
    (east -> eastMap),
)

case class PipeMap(map: Seq[String]) {

    def getRowPositions(rowIdx: Int): Seq[(Int, Int)] = 
        map(rowIdx).zipWithIndex.map(indexedChar => (rowIdx, indexedChar._2))

    def getSymbol(p: (Int, Int)): Char = {
        if (p._1 < 0 || p._2 < 0) {
            groundSymbol
        } else {
            map(p._1)(p._2)
        }
    }

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
def searchPath(p: (Int, Int), direction: Char, map: PipeMap, path: Seq[(Int, Int)] = Seq()): Option[Seq[(Int, Int)]] = {
    val symbol = map.getSymbol(p)
    if (symbol == startPositionSymbol) {
        return Some(path)
    }

    val allowedNextSymbols = directionsMap(direction)
    map.positionFromDirection(p, direction)
        .map(nextPosition => (nextPosition, map.getSymbol(nextPosition)))
        .filter(nextPipe => allowedNextSymbols.keySet.contains(nextPipe._2)) match
            case Some(nextPipe) => {
                val nextDirection = allowedNextSymbols(nextPipe._2)
                searchPath(nextPipe._1, nextDirection, map, path :+ p)
            }
            case _ => None
}

@tailrec
def searchPipes(positions: Seq[(Int, Int)], directions: Seq[Char], map: PipeMap): Option[Seq[(Int, Int)]] = {
    if (directions.isEmpty) {
        return None
    }

    val curDirection = directions.head
    val curPosition = positions.last

    map.positionFromDirection(curPosition, curDirection)
        .flatMap(nextPosition => searchPath(nextPosition, curDirection, map)) match
            case Some(path) => Some(curPosition +: path)
            case _ => searchPipes(positions, directions.tail, map)
}

def pipelinePath(pipeData: Seq[String], map: PipeMap): Seq[(Int, Int)] = {
    val startPosition = map.findStartPosition() match {
        case Some(position) => position
        case _ => throw new RuntimeException("Starting position not found")
    }

    searchPipes(Seq(startPosition), Seq(north, south, west,  east), map).getOrElse(Seq())
}

@tailrec
def createPipePartsRanges(pipeParts: Seq[(Int, Int)], pipeSections: Map[Int, Seq[(Int, Int)]] = Map()): Map[Int, Seq[(Int, Int)]] = {
    if (pipeParts.isEmpty) {
        return pipeSections
    }

    val startPipePart = pipeParts.head
    val pipeSection = (startPipePart +: pipeParts.tail.takeWhile(pipePart => pipePart._1 == startPipePart._1))
        .sortBy( _._2)
    val pipeSectionRange = (pipeSection.head._2, pipeSection.last._2)


    val allPipeSections = pipeSections.getOrElse(startPipePart._1, Seq()) :+ pipeSectionRange
    val newPipeSections = pipeSections + (startPipePart._1 -> allPipeSections)
    createPipePartsRanges(pipeParts.slice(pipeSection.length, pipeParts.length), newPipeSections)
}

def findNestsInRow(pipeParts: Seq[(Int, Int)], rowIdx: Int, map: PipeMap): Seq[(Int, Int)] = {
    if (pipeParts.length < 2) {
        return Seq()
    }

    val rowFields = map.getRowPositions(rowIdx)
    rowFields
        .filter(indexedField => map.getSymbol(indexedField) == groundSymbol)
        .foldLeft(Seq[(Int, Int)]())((results, field) => {
            val pipesCount = rowFields
                                .slice(0, field._2)
                                .filter(field => pipeParts.contains(field))
                                .length
            if (pipesCount % 2 == 0) {
                results
            } else {
                results :+ field
            }
        })
}

def findNestsCount(path: Seq[(Int, Int)], map: PipeMap) = {
    println(createPipePartsRanges(path))
    path.groupBy( _._1 ).map(pair => findNestsInRow(pair._2, pair._1, map).length).sum
}
@main def day10Solution: Unit = {
    Using.Manager { use =>
        
        val pipeData = use(Source.fromResource("day10/smallInput2.txt")).getLines().toSeq
        val map = new PipeMap(pipeData)
        val path = pipelinePath(pipeData, map)

        val path1 = path.tail.zipWithIndex
        val path2 = path.tail.reverse.zipWithIndex
        val farthestPipelinePoint = path1.intersect(path2).map( _._2 ).head + 1

        println(f"Farthest pipeline point: ${farthestPipelinePoint}")

        println(findNestsCount(path, map))
    }
}