package de.kaibra.aoc2021

import scala.annotation.tailrec
import scala.io.Source

object Day12 {

  private val LOWER = "^[a-z]+$".r

  implicit class Expansion(path: Seq[String]) {

    def expand(targetLookup: Map[String, Set[String]],
               canVisitFn: (Seq[String], String) => Boolean): Seq[Seq[String]] =
      targetLookup.get(path.last)
        .map(_.toSeq.flatMap((target: String) =>
          if (canVisitFn(path, target)) Some(path :+ target) else None
        )).getOrElse(Seq())

  }

  def isFinished(p: Seq[String]): Boolean = p.last == "end"

  def canVisitA(s: Seq[String], target: String): Boolean = !s.contains(target) || !LOWER.matches(target)

  def canVisitB(s: Seq[String], target: String): Boolean =
    canVisitA(s, target) || !s.filter(LOWER.matches).groupBy(identity).exists(_._2.length == 2)

  def lookupMap(input: Seq[String], simpleStartAndEnd: Boolean = false): Map[String, Set[String]] = input
    .map(row => row.split("-"))
    .map { case Array(a, b) => (a, b) }
    .foldLeft(Map[String, Set[String]]())((m, e) => {
      var resultM = m
      if (!simpleStartAndEnd || (e._1 != "end" && e._2 != "start"))
        resultM = resultM + (e._1 -> (m.getOrElse(e._1, Set[String]()) + e._2))

      if (!simpleStartAndEnd || (e._1 != "start" && e._2 != "end"))
        resultM = resultM + (e._2 -> (m.getOrElse(e._2, Set[String]()) + e._1))
      resultM
    })

  @tailrec def recurPaths(expandFn: Seq[String] => Seq[Seq[String]],
                          openPaths: Seq[Seq[String]],
                          count: Int = 0): Int = {
    if (openPaths.isEmpty) return count
    val (newClosed, newOpen) = openPaths.foldLeft((Seq[Seq[String]](), Seq[Seq[String]]())) {
      case ((nc, no), nextPath) =>
        if (isFinished(nextPath)) (nc :+ nextPath, no) else (nc, no :++ expandFn(nextPath))
    }
    recurPaths(expandFn, newOpen, count + newClosed.size)
  }

  def solutionA(input: Seq[String]): Int = {
    val lookup = lookupMap(input)
    recurPaths(_.expand(lookup, canVisitA), Seq(Seq("start")))
  }

  def solutionB(input: Seq[String]): Int = {
    val lookup = lookupMap(input, simpleStartAndEnd = true)
    recurPaths(_.expand(lookup, canVisitB), Seq(Seq("start")))
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day12_input.txt").getLines.toSeq
    println(solutionA(input))
    println(solutionB(input))
  }
}
