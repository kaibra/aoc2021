package de.kaibra.aoc2021

import de.kaibra.aoc2021.Day14.Template.calcQuantityDiff

import scala.collection.immutable.Iterable
import scala.io.Source

object Day14 {

  implicit class MergeOps[T](val m: Iterable[(T, Long)]) {
    def mergeToMap: Map[T, Long] = m.foldLeft(Map[T, Long]()) { (m, n) => m + (n._1 -> (m.getOrElse(n._1, 0L) + n._2)) }
  }

  class Template(val entries: Map[String, Long],
                 val results: Map[Char, Long])
                (implicit val mappings: Map[String, Char]) {

    case class ExpandedEntry(entries: Seq[(String, Long)],
                             result: Option[(Char, Long)])

    private def expandEntry(t: (String, Long))(implicit mappings: Map[String, Char]): ExpandedEntry = {
      mappings.get(t._1)
        .map(m => ExpandedEntry(Seq(s"${t._1.head}$m" -> t._2, s"$m${t._1.last}" -> t._2), Some(m -> t._2)))
        .getOrElse(ExpandedEntry(Seq(t), None))
    }

    def expand: Template = {
      val expanded = entries.map(expandEntry)
      val newResults = (expanded.flatMap(_.result) ++ results).mergeToMap
      val newEntries = expanded.flatMap(_.entries).mergeToMap
      new Template(newEntries, newResults)(mappings)
    }

    def quantityDiff: Long = {
      val o = results.toSeq.sortBy(_._2)
      o.last._2 - o.head._2
    }

  }

  object Template {
    def apply(input: String)(implicit mappings: Map[String, Char]): Template = {
      val entries = input.sliding(2).map(t => t -> 1L).toSeq.mergeToMap
      val results = input.map(t => t -> 1L).mergeToMap
      new Template(entries, results)(mappings)
    }

    def calcQuantityDiff(input: Seq[String], iterations: Int): Long = {
      implicit val mappings: Map[String, Char] = input.drop(2).map(_.split(" -> "))
        .map { case Array(p, c) => p -> c.head }.toMap

      var template = Template(input.head)
      for (_ <- 0 until iterations) template = template.expand
      template.quantityDiff
    }
  }

  def solutionA(input: Seq[String]): Long = calcQuantityDiff(input, 10)

  def solutionB(input: Seq[String]): Long = calcQuantityDiff(input, 40)

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day14_input.txt").getLines.toSeq
    println(solutionA(input))
    println(solutionB(input))
  }

}
