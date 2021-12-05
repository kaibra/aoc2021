package de.kaibra.aoc2021

import scala.io.Source
import scala.util.matching.Regex

object Day5a {

  val COORDINATES: Regex = "^(\\d+),(\\d+)\\s*->\\s*(\\d+),(\\d+)$".r

  def range(p1: Int, p2: Int): Range = Math.min(p1, p2) to Math.max(p1, p2)

  def day5SolutionA(input: Seq[String]): Int = {
    input
      .map { case COORDINATES(x1, y1, x2, y2) => (x1.toInt, y1.toInt, x2.toInt, y2.toInt) }
      .flatMap {
        case (x1, y1, x2, y2) =>
          if (x1 == x2) range(y1, y2).map((x1, _))
          else if (y1 == y2) range(x1, x2).map((_, y1))
          else List.empty
      }
      .foldLeft(Map[(Int, Int), Int]()) { (m, n) => m + (n -> (m.getOrElse(n, 0) + 1)) }
      .values.count(_ > 1)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day5_input.txt").getLines.toSeq
    println(day5SolutionA(input))
  }
}
