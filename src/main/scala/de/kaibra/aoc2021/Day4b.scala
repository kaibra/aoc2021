package de.kaibra.aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day4b {

  def day4SolutionB(input: Seq[String]): Int = {
    val numbers = Day4a.getNumbers(input)
    val boards = Day4a.getBoards(input)

    @tailrec def play(ns: Seq[Int], bs: Seq[Board]): Option[Int] = {
      if (ns.isEmpty)
        return None
      val bingos = bs.map(b => b.next(ns.head)).filter(_.isBingo)
      if(bingos.length == bs.length)
        return Some(bingos.last.score())
      play(ns.drop(1), bs.filter(!bingos.contains(_)))
    }

    play(numbers, boards).getOrElse(-1)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day4_input.txt").getLines.toSeq
    println(day4SolutionB(input))
  }
}
