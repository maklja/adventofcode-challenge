import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

case class PathIterator(val path: String) {
    private var pathIterator = path
    private var counter = 0

    def getCounter() = counter

    def next(): Char = {
        val curTurn = pathIterator.head
        pathIterator = pathIterator.tail + pathIterator.head
        counter += 1
        curTurn
    }
}

@tailrec
def parserNetworkNode(nodesData: Seq[String], 
                      networkNode: Map[String, (String, String)] = Map()):  Map[String, (String, String)] = {
    if (nodesData.isEmpty) {
        return networkNode
    }

    val s"$sourceNode = ($leftNode, $rightNode)" = nodesData.head : @unchecked
    parserNetworkNode(nodesData.tail, networkNode  + (sourceNode -> (leftNode, rightNode)))
}

val startNode = "AAA"
val endNode = "ZZZ"

@tailrec
def findPath(curNode: String, pathIterator: PathIterator, network: Map[String, (String, String)]): Int = {
    if (curNode.equals(endNode)) {
        return pathIterator.getCounter()
    }

    val nextNodes = network(curNode)
    val nextNode = pathIterator.next() match
        case 'L' => nextNodes._1
        case _ => nextNodes._2

    findPath(nextNode, pathIterator, network)
}

def processNetworkData(networkData: Seq[String]) = {
    val pathIterator = PathIterator(networkData(0))
    val network = parserNetworkNode(networkData.slice(2, networkData.length))

    findPath(startNode, pathIterator, network)
}

@main def day8Solution: Unit = {
    Using.Manager { use =>
        
        val networkData = use(Source.fromResource("day8/input.txt")).getLines().toSeq
        val pathSteps = processNetworkData(networkData)

        println(f"Total path steps: ${pathSteps}")
    }
}