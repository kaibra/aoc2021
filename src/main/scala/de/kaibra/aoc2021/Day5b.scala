package de.kaibra.aoc2021

import de.kaibra.aoc2021.Day5a.{VERTICAL, RangeCreator, HORIZONTAL, range}

import scala.io.Source

object Day5b {

  val DIAGONAL: RangeCreator = {
    case (x1, y1, x2, y2) if Math.abs(x1 - x2) == Math.abs(y1 - y2) =>
      range(x1, x2).zip(range(y1, y2))
  }

  def day5SolutionB(input: Seq[String]): Int = {
    Day5a.day5SolutionA(input, VERTICAL, HORIZONTAL, DIAGONAL)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day5_input.txt").getLines.toSeq
    println(day5SolutionB(input))
  }
}
