package de.kaibra.aoc2021

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day15 {

  case class Node(riskLevel: Int,
                  x: Int, y: Int,
                  var neighbours: Seq[Node] = Seq(),
                  var visited: Boolean = false,
                  var distance: Int = Integer.MAX_VALUE) {
    override def toString: String = s"Node($x/$y, risk=$riskLevel, distance=$distance)"
  }

  def findNeighbours(node: Node, nodes: Map[(Int, Int), Node]): Seq[Node] = {
    var n = Seq[Node]()
    n = n :++ nodes.get((node.x - 1, node.y))
    n = n :++ nodes.get((node.x + 1, node.y))
    n = n :++ nodes.get((node.x, node.y - 1))
    n = n :++ nodes.get((node.x, node.y + 1))
    n
  }

  def parseNodes(input: Seq[Seq[Int]]): Map[(Int, Int), Node] = {
    val nodes = input.zipWithIndex.flatMap {
      case (row, y) => row.zipWithIndex.map {
        case (cell, x) => (x, y) -> Node(cell, x, y)
      }
    }.toMap
    nodes.map {
      case (point, node) =>
        node.neighbours = findNeighbours(node, nodes)
        point -> node
    }
  }

  def nodeWithShortestDistance(nodes: Iterable[Node]): Option[Node] =
    if (nodes.isEmpty) None else Some(nodes.min((a: Node, b: Node) => a.distance.compareTo(b.distance)))

  def findShortestDist(input: Seq[Seq[Int]]): Int = {
    val nodes: Map[(Int, Int), Node] = parseNodes(input)
    val targetNode: Node = nodes((input.size - 1, input.size - 1))
    val startNode: Node = nodes((0, 0))
    startNode.distance = 0

    @tailrec def calcTargetDistance(unvisited: ArrayBuffer[Node] = ArrayBuffer(startNode)): Int = {
      val current: Option[Node] = nodeWithShortestDistance(unvisited)
      if (current.isEmpty || targetNode.visited) {
        return targetNode.distance
      }
      val currentNode = current.get
      currentNode.visited = true
      unvisited.remove(unvisited.indexOf(currentNode))
      for (neighbour <- currentNode.neighbours.filter(!_.visited)) {
        val dist = neighbour.riskLevel + currentNode.distance
        if (dist < neighbour.distance) {
          neighbour.distance = dist
          unvisited.addOne(neighbour)
        }
      }
      calcTargetDistance(unvisited)
    }

    calcTargetDistance()
  }

  def solutionA(input: Seq[String]): Int =
    findShortestDist(input.map(_.split("").map(_.toInt)))

  def incRow(s: Seq[Int], i: Int = 1): Seq[Int] = s.map(a => Math.max(1, (a + i) % 10))

  def incMatrix(s: Seq[Seq[Int]], i: Int = 1): Seq[Seq[Int]] = s.map(incRow(_, i))

  def solutionB(input: Seq[String], size: Int = 5): Int = {
    val ints = input.map(_.split("").map(_.toInt))
    val extendedRows = ints.map(row => (1 until size).foldLeft((row.toSeq, row.toSeq))((a, _) => {
      val next = incRow(a._1)
      (next, a._2 :++ next)
    })._2)
    val biggerInput = (1 until size).foldLeft((extendedRows, extendedRows))((a, _) => {
      val next = incMatrix(a._1)
      (next, a._2 :++ next)
    })._2
    findShortestDist(biggerInput)
  }

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day15_input.txt").getLines.toSeq
    println(solutionA(input))
    println(solutionB(input))
  }

}
