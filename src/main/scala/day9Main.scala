import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

def parseSensorData(sensorData: Seq[String]): Seq[Seq[Long]] = 
    sensorData.map(curSensorData => curSensorData.split(' ').map( _.toLong ))

@tailrec
def calcSensorHistoryValues(sensorOutput: Seq[Long], sensorHistory: Seq[Seq[Long]] = Seq()): Seq[Seq[Long]] = {
    val historySensorOutput = sensorOutput.sliding(2)
                                            .map(sensorOutputValues => sensorOutputValues(1) - sensorOutputValues(0))
                                            .toSeq

    if (historySensorOutput.sum == 0) {
       return historySensorOutput +: sensorHistory
    }

    calcSensorHistoryValues(historySensorOutput, historySensorOutput +: sensorHistory)
}

@tailrec
def calcExtrapolatedValue(historySensorOutputs: Seq[Seq[Long]]): Long = {
    if (historySensorOutputs.length == 1) {
        return historySensorOutputs(0).last
    }

    val extrapolatedValue = historySensorOutputs(0).last + historySensorOutputs(1).last
    val outputWithExtrapolatedValue = historySensorOutputs(1) :+ extrapolatedValue
    val remainingHistorySensorOutputs = outputWithExtrapolatedValue +: historySensorOutputs.slice(2, historySensorOutputs.length)
    calcExtrapolatedValue(remainingHistorySensorOutputs)
}

def processSensorData(sensorData: Seq[String]) = {
    val sensorOutputs = parseSensorData(sensorData)

    sensorOutputs.map(sensorOutput => {
        val historyValues = calcSensorHistoryValues(sensorOutput, Seq(sensorOutput))
        calcExtrapolatedValue(historyValues)
    }).sum
}

@main def day9Solution: Unit = {
    Using.Manager { use =>
        
        val sensorData = use(Source.fromResource("day9/input.txt")).getLines().toSeq
        val extrapolatedValuesSum = processSensorData(sensorData)

        println(f"Extrapolated values sum: ${extrapolatedValuesSum}")
    }
}