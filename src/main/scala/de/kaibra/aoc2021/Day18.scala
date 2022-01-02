package de.kaibra.aoc2021

import java.util.UUID
import scala.annotation.tailrec
import scala.io.Source

object Day18 {

  trait Node {
    self =>
    val id: UUID = UUID.randomUUID()
    var parent: Option[Pair] = None

    def magnitude: Long

    def replaceWith[T <: Node](replacement: T): T = {
      require(self.parent.isDefined)
      self.parent match {
        case Some(p: Pair) if p.left.id == self.id => p.left = replacement
        case Some(p: Pair) if p.right.id == self.id => p.right = replacement
      }
      replacement.parent = self.parent
      replacement
    }
  }

  class PairVal(var value: Int) extends Node {
    def add(n: Int): Unit = value += n

    override def equals(o: Any): Boolean = o match {
      case p: PairVal => value == p.value
      case _ => false
    }

    override def magnitude: Long = value

    override def toString: String = String.valueOf(value)
  }

  class Pair(var left: Node, var right: Node) extends Node with PairOperations {
    override def toString: String = s"[$left,$right]"

    override def equals(o: Any): Boolean = o match {
      case p: Pair => left.equals(p.left) && right.equals(p.right)
      case _ => false
    }

    override def magnitude: Long = 3 * left.magnitude + 2 * right.magnitude
  }

  trait PairOperations {
    self: Pair =>

    @tailrec private def traverse[T <: Node](node: Node,
                                             spotter: PartialFunction[(Node, Int), T],
                                             level: Int = 0,
                                             visited: Set[UUID] = Set()): Option[T] = node match {
      case n: Node if spotter.isDefinedAt((n, level)) => Some(spotter.apply((n, level)))
      case p: Pair if !visited.contains(p.left.id) => traverse(p.left, spotter, level + 1, visited + node.id)
      case p: Pair if !visited.contains(p.right.id) => traverse(p.right, spotter, level + 1, visited + node.id)
      case n if n.parent.isDefined => traverse(n.parent.get, spotter, level - 1, visited + node.id)
      case _ => None
    }

    def findExplodable: Option[Pair] = traverse[Pair](self, { case (p: Pair, level) if p.isLeaf && level >= 4 => p })

    def findSplittable: Option[PairVal] = traverse[PairVal](self, { case (p: PairVal, _) if p.value >= 10 => p })

    def isLeaf: Boolean = left.isInstanceOf[PairVal] && right.isInstanceOf[PairVal]

    @tailrec private def findParent(parent: Node, child: Node, condition: PartialFunction[(Node, Node), Node]): Option[Node] = {
      if (condition.isDefinedAt(parent, child)) Some(condition.apply(parent, child))
      else if (parent.parent.isEmpty) None
      else findParent(parent.parent.get, parent, condition)
    }

    @tailrec private def findChild(node: Node, nextFn: PartialFunction[Node, Node]): Option[PairVal] = node match {
      case n if nextFn.isDefinedAt(n) => findChild(nextFn.apply(n), nextFn)
      case pv: PairVal => Some(pv)
      case _ => None
    }

    private def findLeftNeighbour(node: Node): Option[PairVal] =
      findParent(node.parent.get, node, { case (p: Pair, c) if p.left.id != c.id => p.left })
        .flatMap(findChild(_, { case p: Pair => p.right }))

    private def findRightNeighbour(node: Node): Option[PairVal] =
      findParent(node.parent.get, node, { case (p: Pair, c) if p.right.id != c.id => p.right })
        .flatMap(findChild(_, { case p: Pair => p.left }))

    private def explode(p: Pair): PairVal = {
      for (leftNeighbour <- findLeftNeighbour(p)) leftNeighbour add p.left.asInstanceOf[PairVal].value
      for (rightNeighbour <- findRightNeighbour(p)) rightNeighbour add p.right.asInstanceOf[PairVal].value
      p.replaceWith(new PairVal(0))
    }

    private def split(splittable: PairVal): Pair = {
      val half = splittable.value.toDouble / 2
      splittable.replaceWith(Pair(Math.floor(half), Math.ceil(half)))
    }

    def reduceSingleStep: Option[Node] = {
      for (explodable <- findExplodable) return Some(explode(explodable))
      for (splittable <- findSplittable) return Some(split(splittable))
      None
    }

    def reduce: Pair = {
      while (reduceSingleStep.isDefined) {}
      self
    }

    def +(p: Any): Pair = Pair(this, Node.toNode(p)).reduce
  }


  object Pair {
    def apply(input: String): Pair = Node.parse(input).asInstanceOf[Pair]

    def apply(left: Node, right: Node): Pair = newInstance(left, right)

    def apply(left: Any, right: Any): Pair = newInstance(left, right)

    private def newInstance(left: Any, right: Any): Pair = {
      val pair = new Pair(Node.toNode(left), Node.toNode(right))
      pair.left.parent = Some(pair)
      pair.right.parent = Some(pair)
      pair
    }
  }

  object Node {
    def toNode(v: Any): Node = v match {
      case i: Int => new PairVal(i)
      case i: Double => new PairVal(i.toInt)
      case n: Node => n
    }

    def split(input: String): (String, String) = {
      if (input.startsWith("[")) {
        val value = input.scanLeft(0) {
          case (s, '[') => s + 1
          case (s, ']') => s - 1
          case (s, _) => s
        }.drop(1)
        val n = value.zipWithIndex.find(_._1 == 0).head._2 + 1
        (input.substring(0, n), input.substring(n + 1))
      } else {
        val n = input.indexOf(',')
        (input.substring(0, n), input.substring(n + 1))
      }
    }

    def parse(row: String): Node = row match {
      case r if r.startsWith("[") =>
        val (left, right) = split(r.substring(1, r.length - 1))
        Pair(parse(left), parse(right))
      case r if r.matches("^\\d+$") => new PairVal(Integer.parseInt(r))
    }

  }

  def solutionA(input: Seq[String]): Long = input.map(Pair(_)).reduceLeft(_ + _).magnitude

  def solutionB(input: Seq[String]): Long = input
    .flatMap(i1 => input.map(i2 => (i1, i2)))
    .map(t => Pair(t._1) + Pair(t._2))
    .map(_.magnitude)
    .max

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day18_input.txt").getLines.toSeq
    println(solutionA(input))
    println(solutionB(input))
  }

}
