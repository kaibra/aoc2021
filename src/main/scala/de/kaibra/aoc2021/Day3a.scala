package de.kaibra.aoc2021

import scala.io.Source

object Day3a {

  def getRates(input: Seq[String]): Option[(Int, Int)] = {
    if (input.isEmpty) return None
    val gammaRateStr = input
      .map(_.split("").map(_.toInt))
      .transpose
      .map(_.sum)
      .map(s => if (s >= input.length.toDouble / 2) 1 else 0)
      .mkString

    val gammaRate = Integer.parseInt(gammaRateStr, 2)
    val epsilonRate = (scala.math.pow(2, input.head.length) - 1).toInt & ~gammaRate
    Some((gammaRate, epsilonRate))
  }

  def day3SolutionA(input: Seq[String]): Int = {
    getRates(input)
      .map(s => s._1 * s._2)
      .getOrElse(0)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day3_input.txt").getLines.toSeq
    println(day3SolutionA(input))
  }
}
