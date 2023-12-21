import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

val topBottom = '|'
val leftRight = '-'
val topRight = 'L'
val topLeft = 'J'
val bottomLeft = '7'
val bottomRight = 'F'
val ground = '.'
val startPosition = 'S'

val states: Map[Char, Seq[Char]] = Map(
    (topBottom -> )
)

@main def day10Solution: Unit = {
    Using.Manager { use =>
        
        val sensorData = use(Source.fromResource("day10/smallInput.txt")).getLines().toSeq
    
    }
}