import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

case class PathIterator(val path: String) {
    private var pathIterator = path
    private var counter = 0L

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

@tailrec
def findPathPattern(curNode: String, pathIterator: PathIterator, network: Map[String, (String, String)]): Long = {
    if (curNode.last == 'Z') {
        return pathIterator.getCounter()
    }

    val nextNodes = network(curNode)
    val nextNode = pathIterator.next() match
        case 'L' => nextNodes._1
        case _ => nextNodes._2

    findPathPattern(nextNode, pathIterator, network)
}

def processNetworkData(networkData: Seq[String]): Long = {
    val network = parserNetworkNode(networkData.slice(2, networkData.length))

    val startNodes = network.keySet.filter(node => node.last == 'A').toSeq
    val pathPatterns = startNodes.map(node => {
        val pathIterator = PathIterator(networkData(0))
        findPathPattern(node, pathIterator, network)
    })

    val max = pathPatterns.max
    val remainingPathPatterns = pathPatterns.filter(pathPattern => pathPattern != max)
    var commonPathPattern = max
    while (!remainingPathPatterns.forall(curPathPattern => commonPathPattern % curPathPattern == 0L)) {
        commonPathPattern = commonPathPattern + max
    }

    commonPathPattern
}

@main def day8Solution: Unit = {
    Using.Manager { use =>
        
        val networkData = use(Source.fromResource("day8/input.txt")).getLines().toSeq
        val pathSteps = processNetworkData(networkData)

        println(f"Total path steps: ${pathSteps}")
    }
}