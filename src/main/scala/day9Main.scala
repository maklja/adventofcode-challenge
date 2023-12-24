import scala.io.Source
import scala.util.Using
import scala.annotation.tailrec

object Day9Challenge:

  @tailrec
  def getSensorHistoryValues(
      sensorOutput: Seq[Long],
      sensorHistory: Seq[Seq[Long]] = Seq()
  ): Seq[Seq[Long]] =
    val historySensorOutput = sensorOutput
      .sliding(2)
      .map(sensorOutputValues => sensorOutputValues(1) - sensorOutputValues(0))
      .toSeq

    if (historySensorOutput.forall(historyValue => historyValue == 0L)) {
      return historySensorOutput +: sensorHistory
    }

    getSensorHistoryValues(
      historySensorOutput,
      historySensorOutput +: sensorHistory
    )

  @tailrec
  def rightExtrapolatedValues(historySensorOutputs: Seq[Seq[Long]]): Long =
    if (historySensorOutputs.length == 1) {
      return historySensorOutputs(0).last
    }

    val extrapolatedValue =
      historySensorOutputs(0).last + historySensorOutputs(1).last
    val outputWithExtrapolatedValue =
      historySensorOutputs(1) :+ extrapolatedValue
    val remainingHistorySensorOutputs =
      outputWithExtrapolatedValue +: historySensorOutputs.slice(
        2,
        historySensorOutputs.length
      )
    rightExtrapolatedValues(remainingHistorySensorOutputs)

  @tailrec
  def leftExtrapolatedValues(historySensorOutputs: Seq[Seq[Long]]): Long =
    if (historySensorOutputs.length == 1) {
      return historySensorOutputs(0).head
    }

    val extrapolatedValue =
      historySensorOutputs(1).head - historySensorOutputs(0).head
    val outputWithExtrapolatedValue =
      extrapolatedValue +: historySensorOutputs(1)
    val remainingHistorySensorOutputs =
      outputWithExtrapolatedValue +: historySensorOutputs.slice(
        2,
        historySensorOutputs.length
      )
    leftExtrapolatedValues(remainingHistorySensorOutputs)

  def processSensorData(sensorData: Seq[String]) =
    val parsedSensorData =
      sensorData.map(_.split(' ').map(_.toLong))
    val historyValues = parsedSensorData.map(sensorOutput =>
      getSensorHistoryValues(sensorOutput, Seq(sensorOutput))
    )

    val rightExtrapolatedValue = historyValues
      .map(rightExtrapolatedValues)
      .sum

    val leftExtrapolatedValue = historyValues
      .map(leftExtrapolatedValues)
      .sum

    (leftExtrapolatedValue, rightExtrapolatedValue)

  @main def day9Main(): Unit =
    Using.Manager { use =>

      val sensorData =
        use(Source.fromResource("day9/input.txt")).getLines().toSeq
      val extrapolatedValuesSum = processSensorData(sensorData)

      println(f"Extrapolated left values sum: ${extrapolatedValuesSum._1}")
      println(f"Extrapolated right values sum: ${extrapolatedValuesSum._2}")
    }

end Day9Challenge
