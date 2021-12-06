
package de.kaibra.aoc2021

import de.kaibra.aoc2021.Day6a.day6SolutionA

import scala.io.Source

object Day6b {

  def day6SolutionB(input: Seq[String],
                    maxIterations: Int = 80): BigInt = {
    day6SolutionA(input, maxIterations)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day6_input.txt").getLines.toSeq
    println(day6SolutionB(input, 256))
  }
}
