
package de.kaibra.aoc2021

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap
import scala.io.Source

object Day6a {

  def day6SolutionA(input: Seq[String],
                    maxIterations: Int = 80): BigInt = {

    val fishs = input.head.split(",").map(_.toInt)
      .foldLeft(SortedMap[Int, Int](Seq.fill(9)(0).zipWithIndex.map(e => e._2 -> e._1): _*)) {
        (m, n) => m + (n -> (m.getOrElse(n, 0) + 1))
      }.values.toSeq.map(BigInt.apply)

    @tailrec def grow(f: Seq[BigInt], n: Int): BigInt = {
      if (n == maxIterations)
        return f.sum
      val (Seq(head), tail) = f.splitAt(1)
      grow(
        (Seq.fill(6)(BigInt(0)) :+ head :+ BigInt(0) :+ head).zipAll(tail, BigInt(0), BigInt(0)).map(t => t._2.+(t._1)),
        n + 1
      )
    }

    grow(fishs, 0)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day6_input.txt").getLines.toSeq
    println(day6SolutionA(input))
  }
}
