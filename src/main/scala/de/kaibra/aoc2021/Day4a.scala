package de.kaibra.aoc2021

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class Board(val numbers: Vector[Int],
            var refs: ArrayBuffer[Int],
            val size: Int) {

  private val AND = (a: Int, b: Int) => a & b
  private var lastNumber: Int = -1

  def next(number: Int): Unit = {
    lastNumber = number
    val index = numbers.indexOf(number)
    if (index >= 0)
      refs(index) = 1
  }

  def isBingo: Boolean = {
    for (rowStart <- 0 until size * size - size by size)
      if (refs.slice(rowStart, rowStart + size).reduce(AND) == 1)
        return true

    for (column <- 0 until size)
      if ((for (row <- 0 until size) yield refs(column + row * size)).reduce(AND) == 1)
        return true

    false
  }

  def score(): Int = {
    numbers.zip(refs).filter(_._2 == 0).map(_._1).sum * lastNumber
  }

}

object Board {
  def apply(input: Seq[String]): Board = {
    val nums = input.flatMap(_.trim.split("\\s+").map(_.toInt)).toVector
    new Board(nums, ArrayBuffer.fill(nums.length)(0), input.length)
  }
}

object Day4a {

  def day4SolutionA(input: Seq[String]): Int = {
    val numbers = getNumbers(input)
    val boards = getBoards(input)

    def play: Option[Int] = {
      for (n <- numbers)
        for (b <- boards) {
          b.next(n)
          if (b.isBingo) {
            return Some(b.score())
          }
        }
      None
    }

    play.getOrElse(-1)
  }

  private def getBoards(input: Seq[String]) = {
    val splitIndexes = input
      .zipWithIndex.filter(_._1.isEmpty)
      .map(_._2) :+ input.length
    splitIndexes
      .sliding(2)
      .map { case List(start, end) => Board(input.slice(start + 1, end)) }
      .toSeq
  }

  private def getNumbers(input: Seq[String]) = {
    input.head.split(",").map(_.toInt).toSeq
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day4_input.txt").getLines.toSeq
    println(day4SolutionA(input))
  }
}
