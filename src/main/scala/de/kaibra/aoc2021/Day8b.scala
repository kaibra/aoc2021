
package de.kaibra.aoc2021

import scala.io.Source

object Day8b {

  def getDecodedObservations(obs: Seq[Set[Char]]): Seq[(Int, Set[Char])] = {
    def byLength(l: Int): Set[Char] = obs.find(_.size == l).get

    val one = (1, byLength(2))
    val four = (4, byLength(4))
    val seven = (7, byLength(3))
    val eight = (8, byLength(7))

    val Seq(zero, six, nine) = obs.filter(_.size == 6).map {
      case o if o.intersect(four._2) == four._2 => (9, o)
      case o if o.intersect(one._2) == one._2 => (0, o)
      case o => (6, o)
    }.sortWith((a, b) => a._1 < b._1)

    val Seq(two, three, five) = obs.filter(_.size == 5).map {
      case o if o.intersect(one._2) == one._2 => (3, o)
      case o if o.intersect(nine._2 -- one._2).size == 4 => (5, o)
      case o => (2, o)
    }.sortWith((a, b) => a._1 < b._1)

    Seq(zero, one, two, three, four, five, six, seven, eight, nine)
  }

  def decode(input: String): Long = {
    val Seq(obs, code) = input.split("\\s+\\|\\s+").toSeq
      .map(e => e.split("\\s+").map(c =>
        Set(c.split("").toSeq.map(_.head): _*)))

    val decodedObs = getDecodedObservations(obs)

    code.map(c => decodedObs.find(d => d._2 == c).get._1)
      .mkString.toInt
  }

  def day8SolutionB(input: Seq[String]): Long = {
    input.map(decode).sum
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day8_input.txt").getLines.toSeq
    println(day8SolutionB(input))
  }
}
