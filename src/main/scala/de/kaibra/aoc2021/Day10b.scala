

package de.kaibra.aoc2021

import de.kaibra.aoc2021.Day10a.{SyntaxInfo, negate, toSyntaxInfo}

import scala.io.Source

object Day10b {

  def pointsOf(c: Char): Long = c match {
    case ')' => 1
    case ']' => 2
    case '}' => 3
    case '>' => 4
  }

  def toScore(input: String): Long = input.foldLeft(0L)((r, c) => (r * 5) + pointsOf(c))

  def day10SolutionB(input: Seq[String]): Long = {
    val results = input
      .flatMap(c => toSyntaxInfo(c) match {
        case SyntaxInfo(s, errors) if errors.isEmpty && s.nonEmpty => Some(s.map(negate).mkString(""))
        case _ => None
      })
      .map(toScore)
      .sorted
    results(results.length / 2)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day10_input.txt").getLines.toSeq
    println(day10SolutionB(input))
  }
}
