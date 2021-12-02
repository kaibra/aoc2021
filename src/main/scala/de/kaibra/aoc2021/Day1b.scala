package de.kaibra.aoc2021

import scala.io.Source

object Day1b {

  def day1SolutionB(input: Seq[Int]): Int = {
    Day1a.day1SolutionA(
      for (i <- 0 until Math.max(0, input.length - 2))
        yield input(i) + input(i + 1) + input(i + 2)
    )
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day1_input.txt").getLines.map(_.toInt).toSeq
    println(day1SolutionB(input))
  }
}
