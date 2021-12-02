package de.kaibra.aoc2021

import scala.io.Source

object Day1a {

  def day1SolutionA(input: Seq[Int]): Int =
    input
      .foldLeft((0, None: Option[Int]))((o, next: Int) => o match {
        case (r, Some(last)) if next > last => (r + 1, Some(next))
        case (r, _) => (r, Some(next))
      })._1

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day1_input.txt").getLines.map(_.toInt).toSeq
    println(day1SolutionA(input))
  }
}
