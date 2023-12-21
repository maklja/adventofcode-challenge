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

    if (historySensorOutput.forall(historyValue => historyValue == 0L)) {
       return historySensorOutput +: sensorHistory
    }

    calcSensorHistoryValues(historySensorOutput, historySensorOutput +: sensorHistory)
}

@tailrec
def calcExtrapolatedValue(historySensorOutputs: Seq[Seq[Long]]): Long = {
    if (historySensorOutputs.length == 1) {
        return historySensorOutputs(0).head
    }

    val extrapolatedValue = historySensorOutputs(1).head -  historySensorOutputs(0).head
    val outputWithExtrapolatedValue = extrapolatedValue +: historySensorOutputs(1)
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