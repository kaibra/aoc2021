package de.kaibra.aoc2021

import scala.io.Source

object Day13 {

  def X_FOLD: Int => ((Int, Int)) => (Int, Int) = xFold => {
    case (x, y) if x > xFold => (xFold - 1 - ((x - 1) % xFold), y)
    case t => t
  }

  def Y_FOLD: Int => ((Int, Int)) => (Int, Int) = yFold => {
    case (x, y) if y > yFold => (x, yFold - 1 - ((y - 1) % yFold))
    case t => t
  }

  def parse(input: Seq[String]): (Seq[(Int, Int)], Seq[((Int, Int)) => (Int, Int)]) = {
    val (strCoords, strFolds) = input.splitAt(input.indexOf(""))
    val coordinates = strCoords.map(_.split(",").map(_.toInt)).map { case Array(x, y) => (x, y) }
    val folds = strFolds.drop(1).map(_.split("=")).map {
      case Array("fold along x", x) => X_FOLD(x.toInt)
      case Array("fold along y", y) => Y_FOLD(y.toInt)
    }
    (coordinates, folds)
  }

  def toReadableOutput(input: Seq[(Int, Int)]): String = {
    (for (y <- (0 to input.map(_._2).max))
      yield (for (x <- (0 to input.map(_._1).max))
        yield input.find(_ == (x, y)).map(_ => "#").getOrElse("."))
        .mkString("")).mkString("\n")
  }

  def solutionA(input: Seq[String]): Int = {
    val (coordinates, folds) = parse(input)
    coordinates.map(t => folds.head(t)).toSet.size
  }

  def solutionB(input: Seq[String]): String = {
    val (coordinates, folds) = parse(input)
    val mappedCoordinates = coordinates.map(t => folds.foldLeft(t)((r, n) => n(r)))
    toReadableOutput(mappedCoordinates)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day13_input.txt").getLines.toSeq
    println(solutionA(input))
    println(solutionB(input))
  }
}
