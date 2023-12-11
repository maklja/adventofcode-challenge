import scala.io.Source
import scala.util.Using

case class Position(x: Int, y: Int)

enum EngineSchemaType: 
   case EnginePart, Symbol, Empty

case class EngineSchemaEntity(value: String, `type`: EngineSchemaType, position: Position) {
    def containsPosition(p: Position): Boolean = {
        val entityPositions = generatePositions(position, value.size)
        entityPositions.exists(curPosition => curPosition.equals(p))
    }

    private def generatePositions(p: Position, size: Int): Seq[Position] = {
        val prevRow = p.x - 1
        val nextRow = p.x + 1

        val columnRange = Range.inclusive(p.y - 1, p.y + size)
        val prevRowPositions = columnRange.map(colIdx => Position(prevRow, colIdx))
        val nextRowPositions = columnRange.map(colIdx => Position(nextRow, colIdx))
        val leftPosition = Position(p.x, p.y - 1)
        val rightPosition = Position(p.x, p.y + size)

        prevRowPositions ++ nextRowPositions :+ leftPosition :+ rightPosition
    }
}

def hasHandlerMatch(c: Char): EngineSchemaType =
    if (c.isDigit) {
        EngineSchemaType.EnginePart
    } else if (c != '.') {
        EngineSchemaType.Symbol
    } else {
        EngineSchemaType.Empty
    }

def handleEnginePart(line: String, colIdx: Int,  rowIdx: Int) = {
    val enginePart = line.takeWhile( _.isDigit )
    EngineSchemaEntity(enginePart, EngineSchemaType.EnginePart, Position(rowIdx, colIdx))
}

def handleSymbol(line: String, colIdx: Int,  rowIdx: Int) = {
    val symbol = line.takeWhile(c => !c.isDigit && c != '.')
    EngineSchemaEntity(symbol, EngineSchemaType.Symbol, Position(rowIdx, colIdx))
}

def parseEngineSchemaLine(line: String, rowIdx: Int, colIdx: Int): Seq[EngineSchemaEntity] = {
    if (line.isEmpty) {
        return  Seq[EngineSchemaEntity]()
    }

    hasHandlerMatch(line.head) match
        case EngineSchemaType.EnginePart => {
            val enginePart = handleEnginePart(line, colIdx, rowIdx)
            val newColIdx = colIdx + enginePart.value.length
            parseEngineSchemaLine(line.slice(enginePart.value.length, line.length), rowIdx, newColIdx) :+ enginePart
        } 
        case EngineSchemaType.Symbol => {
            val symbol = handleSymbol(line, colIdx, rowIdx)
            val newColIdx = colIdx + symbol.value.length
            parseEngineSchemaLine(line.slice(symbol.value.length, line.length), rowIdx, newColIdx) :+ symbol
        }
        case _ => {
            val emptyChars = line.takeWhile { _ == '.' }
            val newColIdx = colIdx + emptyChars.length
            parseEngineSchemaLine(line.slice(emptyChars.length, line.length), rowIdx, newColIdx)
        }
}

def retrieveValidEngineParts(engineParts: Map[Position, EngineSchemaEntity], 
                             symbols: Seq[EngineSchemaEntity]): Seq[Int] = {
    symbols.filter( _.value.equals("*") ).flatMap(symbol => {
        val affectedRows = Seq[Int](symbol.position.x, symbol.position.x -1, symbol.position.x + 1);
        val affectedEngineParts = engineParts.values
                                            .filter(enginePart => affectedRows.contains(enginePart.position.x) && enginePart.containsPosition(symbol.position))
                                            .map( _.value.toInt )
                                            .toSeq
        if (affectedEngineParts.length > 1) {
            val sumValue = affectedEngineParts.reduceLeft((sum, enginePart) => sum * enginePart)
            Seq(sumValue)
        } else {
            Seq()
        }
    })
}

def processEngineSchema(engineSchema: Seq[String]) = {
    var engineParts = Map[Position, EngineSchemaEntity]()
    var symbols = Seq[EngineSchemaEntity]()

    engineSchema.zipWithIndex.foreach((line, i) => {
        val lineEngineSchemaEntities = parseEngineSchemaLine(line, i, 0)
        engineParts ++= lineEngineSchemaEntities.filter { _.`type` == EngineSchemaType.EnginePart }
                                                .foldLeft(Map[Position, EngineSchemaEntity]())((map, entity) => map + (entity.position -> entity))
        symbols ++= lineEngineSchemaEntities.filter { _.`type` == EngineSchemaType.Symbol }
    })

    retrieveValidEngineParts(engineParts, symbols)
}

@main def day3Solution: Unit = {
    Using.Manager { use =>

        val engineSchema = use(Source.fromResource("day3/input.txt")).getLines()
        val correctEnginePartsSum = processEngineSchema(engineSchema.toSeq).sum

        println(f"Engine parts sum: ${correctEnginePartsSum}")
    }
}