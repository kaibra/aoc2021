
package de.kaibra.aoc2021

import scala.io.Source

object Day8a {

  def day8SolutionA(input: Seq[String]): Int = {
    val unique = Set(2, 3, 4, 7)
    input.flatMap(_.split("\\s*\\|\\s*")(1).split("\\s+")
      .map(_.trim.length)).count(unique.contains)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day8_input.txt").getLines.toSeq
    println(day8SolutionA(input))
  }
}
