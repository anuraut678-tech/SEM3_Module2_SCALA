package Prac_2

import scala.io.Source
import breeze.linalg._
import breeze.plot._

object Ananya_Prac_2_TimeSeriesAnalysis {
  def main(args: Array[String]): Unit = {

    val stream =
      getClass.getResourceAsStream("/TCS.NS.csv")

    if (stream == null) {
      println("Error: File not found in resources folder!")
      return
    }

    val file = Source.fromInputStream(stream)

    // Import dataset and extract Close Price
    val closePrice = file.getLines().drop(1).flatMap { line =>
      val cols = line.split(",")

      for {
        price <- cols(4).trim.toDoubleOption
      } yield price

    }.toList

    file.close()

    // Feature Engineering
    val window = 5

    // Simple Moving Average (SMA)
    val sma = closePrice.sliding(window)
      .map(values => values.sum / window)
      .toList

    // Weighted Moving Average (WMA)
    val weights = (1 to window).toList
    val totalWeight = weights.sum.toDouble

    val wma = closePrice.sliding(window).map { values =>
      values.zip(weights)
        .map { case (value, weight) => value * weight }
        .sum / totalWeight
    }.toList

    // Exponential Moving Average (EMA)
    val alpha = 2.0 / (window + 1)

    var ema = List(closePrice.head)

    for (price <- closePrice.tail) {

      val next =
        alpha * price + (1 - alpha) * ema.last

      ema = ema :+ next
    }

    // Data Visualization
    val f = Figure()
    val p = f.subplot(0)

    val xClose = DenseVector(
      (0 until closePrice.length)
        .map(_.toDouble)
        .toArray
    )

    val yClose = DenseVector(closePrice.toArray)

    val xMA = DenseVector(
      (window - 1 until closePrice.length)
        .map(_.toDouble)
        .toArray
    )

    val ySMA = DenseVector(sma.toArray)

    val yWMA = DenseVector(wma.toArray)

    val xEMA = DenseVector(
      (0 until ema.length)
        .map(_.toDouble)
        .toArray
    )

    val yEMA = DenseVector(ema.toArray)

    p += breeze.plot.plot(
      xClose,
      yClose,
      name = "Close Price"
    )

    p += breeze.plot.plot(
      xMA,
      ySMA,
      name = "SMA"
    )

    p += breeze.plot.plot(
      xMA,
      yWMA,
      name = "WMA"
    )

    p += breeze.plot.plot(
      xEMA,
      yEMA,
      name = "EMA"
    )

    p.xlabel = "Days"
    p.ylabel = "TCS Closing Price"
    p.title = "TCS Moving Average Analysis"

    f.refresh()

    // Results
    println("---------- RESULT ----------")

    println(s"Dataset Size: ${closePrice.length} records")

    println(s"Moving Average Window: $window days")

    println("\nFirst 10 SMA Values:")
    sma.take(10).foreach(value =>
      println(f"$value%.2f")
    )

    println("\nFirst 10 WMA Values:")
    wma.take(10).foreach(value =>
      println(f"$value%.2f")
    )

    println("\nFirst 10 EMA Values:")
    ema.take(10).foreach(value =>
      println(f"$value%.2f")
    )

  }
}
