
package de.kaibra.aoc2021

import scala.io.Source

object Day9a {

  case class Day9Data(matrix: Seq[Seq[Int]],
                      indexes: Seq[(Int, Int)],
                      leftNeighbour: Seq[Seq[Int]],
                      rightNeighbour: Seq[Seq[Int]],
                      topNeighbour: Seq[Seq[Int]],
                      bottomNeighbour: Seq[Seq[Int]])

  def calcData(input: Seq[String]): Day9Data = {
    val matrix = input.map(_.split("").map(_.toInt).toSeq)

    def normalize(i: Int): Int = if (i < 0) 1 else 0

    // 1 when neighbour is greater else 0
    val leftNeighbour = matrix.map(1 +: _.sliding(2).map(a => normalize(a.last - a.head)).toSeq)
    val rightNeighbour = matrix.map(_.sliding(2).map(a => normalize(a.head - a.last)).toSeq :+ 1)
    val topNeighbour = matrix.transpose.map(1 +: _.sliding(2).map(a => normalize(a.last - a.head)).toSeq).transpose
    val bottomNeighbour = matrix.transpose.map(_.sliding(2).map(a => normalize(a.head - a.last)).toSeq :+ 1).transpose

    val merged = rightNeighbour.lazyZip(leftNeighbour).lazyZip(topNeighbour).lazyZip(bottomNeighbour)
      .map((a, b, c, d) => a.lazyZip(b).lazyZip(c).lazyZip(d).map((a, b, c, d) => (a + b + c + d) / 4))

    val indexes = merged.zipWithIndex.flatMap(t => t._1.zipWithIndex.filter(_._1 == 1).map(r => (r._2, t._2)))

    Day9Data(matrix, indexes,
      leftNeighbour, rightNeighbour, topNeighbour, bottomNeighbour)
  }

  def day9SolutionA(input: Seq[String]): Int = {
    val m = calcData(input)
    m.indexes.map { case (x, y) => m.matrix(y)(x) + 1 }.sum
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day9_input.txt").getLines.toSeq
    println(day9SolutionA(input))
  }
}
