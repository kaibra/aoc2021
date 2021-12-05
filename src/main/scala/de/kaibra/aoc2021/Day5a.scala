package de.kaibra.aoc2021


import scala.io.Source
import scala.util.matching.Regex

object Day5a {

  type RangeCreator = PartialFunction[(Int, Int, Int, Int), Seq[(Int, Int)]]

  val COORDINATES: Regex = "^(\\d+),(\\d+)\\s*->\\s*(\\d+),(\\d+)$".r

  def range(p1: Int, p2: Int): Range = {
    val min = Math.min(p1, p2)
    val r = min to Math.max(p1, p2)
    if (min == p1) r else r.reverse
  }

  val VERTICAL: RangeCreator = {
    case (x1, y1, x2, y2) if x1 == x2 => range(y1, y2).map((x1, _))
  }

  val HORIZONTAL: RangeCreator = {
    case (x1, y1, x2, y2) if y1 == y2 => range(x1, x2).map((_, y1))
  }

  def day5SolutionA(input: Seq[String],
                    creators: RangeCreator*): Int = {
    val liftedCreators = creators.map(_.lift)
    input
      .map { case COORDINATES(x1, y1, x2, y2) => (x1.toInt, y1.toInt, x2.toInt, y2.toInt) }
      .flatMap(t => liftedCreators.foldLeft(None: Option[Seq[(Int, Int)]])((r, c) => r.orElse(c.apply(t))).getOrElse(Seq()))
      .foldLeft(Map[(Int, Int), Int]()) { (m, n) => m + (n -> (m.getOrElse(n, 0) + 1)) }
      .values.count(_ > 1)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day5_input.txt").getLines.toSeq
    println(day5SolutionA(input, VERTICAL, HORIZONTAL))
  }
}
