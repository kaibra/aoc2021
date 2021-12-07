
package de.kaibra.aoc2021

import scala.io.Source

object Day7b {

  def day7SolutionB(input: Seq[String]): Int = {
    val numbers = input.head.split(",").map(_.toInt).toSeq

    def cost(x: Int): Int =
      numbers.map(t => (1 to Math.abs(t - x)).sum).sum

    (numbers.min to numbers.max)
      .map(cost)
      .min
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day7_input.txt").getLines.toSeq
    println(day7SolutionB(input))
  }
}
