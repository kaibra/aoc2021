package de.kaibra.aoc2021

import scala.io.Source

class Octopus(val x: Int, val y: Int, var energyLevel: Int) {
  private var neighbours: Seq[Octopus] = Seq()

  def connect(other: Seq[Octopus]): Unit = neighbours = other.filter(o => (Math.abs(o.x - x) | Math.abs(o.y - y)) == 1)

  def inc(): Unit = energyLevel += 1

  private def flashed(): Int = {
    if (energyLevel != 0) {
      inc()
      alight()
    } else {
      0
    }
  }

  def alight(): Int = {
    if (energyLevel > 9) {
      energyLevel = 0
      (1 +: (for (n <- neighbours) yield n.flashed())).sum
    } else {
      0
    }
  }
}

class Grid(val octopuses: Seq[Octopus]) {
  def step: Int = {
    octopuses.foreach(_.inc())
    octopuses.map(_.alight()).sum
  }
}

object Grid {
  def apply(input: Seq[String]): Grid = {
    val notConnectedOcto = input.zipWithIndex.flatMap { case (row, y) => row.split("").zipWithIndex.map { case (cell, x) => new Octopus(x, y, cell.toInt) } }
    notConnectedOcto.foreach(_.connect(notConnectedOcto))
    new Grid(notConnectedOcto)
  }
}

object Day11 {

  def solutionA(input: Seq[String]): Int = {
    val grid = Grid(input)
    (for (_ <- 1 to 100) yield grid.step).sum
  }

  def solutionB(input: Seq[String]): Int = {
    val grid = Grid(input)
    var step = 1
    while (grid.step != 100) step += 1
    step
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day11_input.txt").getLines.toSeq
    println(solutionA(input))
    println(solutionB(input))
  }
}
