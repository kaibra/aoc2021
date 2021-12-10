

package de.kaibra.aoc2021

import scala.collection.mutable
import scala.io.Source

object Day10a {

  val OPEN = Set('(', '[', '{', '<')
  val CLOSE = Set(')', ']', '}', '>')

  def negate(c: Char): Char = c match {
    case '(' => ')'
    case '[' => ']'
    case '{' => '}'
    case '<' => '>'
  }

  case class SyntaxInfo(s: mutable.Stack[Char], errors: Seq[Char])

  def toSyntaxInfo(line: String): SyntaxInfo = {
    line.foldLeft(SyntaxInfo(mutable.Stack(), Seq())) {
      case (SyntaxInfo(stack, s), c) if OPEN.contains(c) =>
        SyntaxInfo(stack.push(c), s)
      case (SyntaxInfo(stack, s), c) if CLOSE.contains(c) =>
        if (negate(stack.pop()) == c)
          SyntaxInfo(stack, s)
        else
          SyntaxInfo(stack, s :+ c)
    }
  }

  def toScore(syntaxErrors: Map[Char, Int]): Int = {
    syntaxErrors.map {
      case (c, s) if c == '(' || c == ')' => s * 3
      case (c, s) if c == '[' || c == ']' => s * 57
      case (c, s) if c == '{' || c == '}' => s * 1197
      case (c, s) if c == '<' || c == '>' => s * 25137
    }.sum
  }

  def day10SolutionA(input: Seq[String]): Int = {
    input
      .map(c => toSyntaxInfo(c).errors match {
        case Seq(f) => Map(f -> 1)
        case _ => Map[Char, Int]()
      })
      .map(toScore)
      .sum
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day10_input.txt").getLines.toSeq
    println(day10SolutionA(input))
  }
}
