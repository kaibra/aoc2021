package de.kaibra.aoc2021

import scala.io.Source
import scala.util.matching.Regex

object Day2a {

  private val NAV_PATTERN: Regex = """^(forward|down|up) (\d+)$""".r

  def day2SolutionA(input: Seq[String]): Int = {
    var depth = 0;
    var position = 0;
    input.foreach {
      case NAV_PATTERN("forward", n) => position += n.toInt
      case NAV_PATTERN("down", n) => depth += n.toInt
      case NAV_PATTERN("up", n) => depth -= n.toInt
    }
    depth * position
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day2_input.txt").getLines.toSeq
    println(day2SolutionA(input))
  }
}
