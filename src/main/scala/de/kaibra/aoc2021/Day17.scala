package de.kaibra.aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day17 {

  implicit class Ops(s: (Int, Int)) {
    def includes(x: Int): Boolean = s._1 <= x && x <= s._2

    def toRange: Range = s._1 to s._2
  }

  val TRENCH: Regex = "^target area: x=(-?\\d+)\\.\\.(-?\\d+), y=(-?\\d+)\\.\\.(-?\\d+)$".r

  def parseTrench: PartialFunction[String, ((Int, Int), (Int, Int))] = {
    case TRENCH(x1, x2, y1, y2) => ((x1.toInt, x2.toInt), (y1.toInt, y2.toInt))
  }

  def littleGauss(n: Int): Int = (n * n + n) / 2

  def xReihe(start: Int,
             max: Int = Int.MaxValue): (Seq[Int], Boolean) = {
    @tailrec def reihe(start: Int, result: Seq[Int]): (Seq[Int], Boolean) = {
      if (start == 0) return (result, true)
      if (result.nonEmpty && result.last + start > max) return (result, false)
      reihe(start - 1, result :+ result.lastOption.map(_ + start).getOrElse(start))
    }

    reihe(start, Seq())
  }

  def yReihe(start: Int,
             min: Int = Int.MinValue): Seq[Int] = {
    @tailrec def reihe(start: Int, result: Seq[Int]): Seq[Int] = {
      if (result.nonEmpty && (result.last + start < min)) return result
      reihe(start - 1, result :+ result.lastOption.map(_ + start).getOrElse(start))
    }

    reihe(start, Seq())
  }

  def allMatches(input: Seq[String]): Seq[(Int, Int)] = {
    val (xRange, yRange) = parseTrench(input.head)
    val smallestXVelocity = LazyList.from(1).filter(n => {
      val lg = littleGauss(n)
      lg >= xRange._1 && lg <= xRange._2
    }).take(1).head
    val xVelocityLimits = (smallestXVelocity, xRange._2)
    val yVelocityLimits = (yRange._1, Math.abs(yRange._1) - 1)

    xVelocityLimits.toRange
      .flatMap(xVelocity => yVelocityLimits.toRange.flatMap(yVelocity => {
        val graphInTarget = (xReihe(xVelocity, max = (xVelocityLimits._2)) match {
          case (xr, true) => xr.zipAll(yReihe(yVelocity, min = yVelocityLimits._1), xr.last, Int.MinValue)
          case (xr, false) => xr.zip(yReihe(yVelocity, min = yVelocityLimits._1).toList)
        }).exists(r => xRange.includes(r._1) && yRange.includes(r._2))
        if (graphInTarget) Some((xVelocity, yVelocity)) else None
      }))
  }

  def solutionA(input: Seq[String]): Int = {
    val (_, yRange) = parseTrench(input.head)
    littleGauss(Math.abs(yRange._1) - 1)
  }

  def solutionB(input: Seq[String]): Long = allMatches(input).size

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day17_input.txt").getLines.toSeq
    println(solutionA(input))
    println(solutionB(input))
  }

}
