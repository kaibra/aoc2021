package de.kaibra.aoc2021

import scala.io.Source

object Day3b {

  def asPaddedBinaryString(i: Int, l: Int): String = {
    val s = i.toBinaryString
    "0" * (l - s.length) + s
  }

  private def gammaStr(input: Seq[String]): String = {
    val (gamma, _) = Day3a.getRates(input).get
    asPaddedBinaryString(gamma, input.head.length)
  }

  private def epsilonStr(input: Seq[String]): String = {
    val (_, epsilon) = Day3a.getRates(input).get
    asPaddedBinaryString(epsilon, input.head.length)
  }

  def day3SolutionB(input: Seq[String]): Int = {
    def findNr = (fn: Seq[String] => String) =>
      (0 to input.head.length)
        .foldLeft((input, fn(input))) {
          case (o, _) if o._1.length <= 1 => o
          case ((r, v), i) =>
            val nextR = r.filter(_ (i) == v(i))
            (nextR, fn(nextR))
        }._1

    Integer.parseInt(findNr(gammaStr).head, 2) *
      Integer.parseInt(findNr(epsilonStr).head, 2)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day3_input.txt").getLines.toSeq
    println(day3SolutionB(input))
  }
}
