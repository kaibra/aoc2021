

package de.kaibra.aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day9b {

  def day9SolutionB(input: Seq[String]): Int = {
    val d = Day9a.calcData(input)
    def isInRangeOfMatrix(p: (Int, Int)): Boolean = p._2 >= 0 && p._2 < d.matrix.length && p._1 >= 0 && p._1 < d.matrix.head.length
    def isNotNine(p: (Int, Int)): Boolean = d.matrix(p._2)(p._1) != 9

    @tailrec def traverse2(newPoints: Seq[(Int, Int)], result: Set[(Int, Int)] = Set()): Set[(Int, Int)] = {
      if (newPoints.isEmpty)
        return result

      val filteredPoints = newPoints
        .filter(isInRangeOfMatrix)
        .filter(isNotNine)

      val pointNeighbours = filteredPoints
        .flatMap(p => {
          // mapping points to its neighbours
          var s = Seq[(Int, Int)]()
          // has left neighbour which is higher
          if (d.leftNeighbour(p._2)(p._1) == 1) s = s :+ (p._1 - 1, p._2)
          // has right neighbour which is higher
          if (d.rightNeighbour(p._2)(p._1) == 1) s = s :+ (p._1 + 1, p._2)
          // has top neighbour which is higher
          if (d.topNeighbour(p._2)(p._1) == 1) s = s :+ (p._1, p._2 - 1)
          // has bottom neighbour which is higher
          if (d.bottomNeighbour(p._2)(p._1) == 1) s = s :+ (p._1, p._2 + 1)
          s
        })

      traverse2(pointNeighbours, result ++ filteredPoints)
    }

    d.indexes.map(p => traverse2(Seq(p)))
      .map(_.size).sorted.reverse
      .take(3).product
  }


  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day9_input.txt").getLines.toSeq
    println(day9SolutionB(input))
  }
}
