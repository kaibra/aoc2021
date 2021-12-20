package de.kaibra.aoc2021

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex

object Day16 {

  val PACKAGE_HEADER: Regex = "^([01]{3})([01]{3})$".r

  sealed trait Packet {
    val version: Int
    val id: Int

    def value: Long
  }

  case class Literal(version: Int, id: Int, value: Long) extends Packet

  case class Operator(version: Int, id: Int, lengthId: Int, length: Int, subPackets: Seq[Packet]) extends Packet {
    override def value: Long = id match {
      case 0 => subPackets.map(_.value).sum
      case 1 => subPackets.map(_.value).product
      case 2 => subPackets.map(_.value).min
      case 3 => subPackets.map(_.value).max
      case 5 => if (subPackets.head.value > subPackets.last.value) 1 else 0
      case 6 => if (subPackets.head.value < subPackets.last.value) 1 else 0
      case 7 => if (subPackets.head.value == subPackets.last.value) 1 else 0
    }
  }

  object Operator {
    def apply(version: Int, id: Int, lengthId: Int, length: Int, str: String): Operator =
      new Operator(version, id, lengthId, length, decode(str))

    def isLengthComplete(lengthTypeId: Int, buffer: String): Boolean =
      (lengthTypeId == 0 && buffer.length == 15) || (lengthTypeId == 1 && buffer.length == 11)
  }

  object Literal {
    def apply(version: Int, id: Int, str: String): Literal = {
      val parsed = str.grouped(5).map(_.substring(1)).mkString
      new Literal(version, id, java.lang.Long.parseLong(parsed, 2))
    }

    def isValueComplete(buffer: String): Boolean = buffer.nonEmpty && buffer.length % 5 == 0 && buffer(buffer.length - 5) == '0'
  }

  sealed trait PackageHead {
    val version: Int
    val id: Int
  }

  case class LiteralPackageHead(version: Int, id: Int) extends PackageHead

  case class OperatorPackageHead(version: Int, id: Int, lengthTypeId: Int, length: Int) extends PackageHead

  def decodeHex(hexPattern: String): String =
    hexPattern.map(c => {
      val bin = c.asDigit.toBinaryString
      "0" * (4 - bin.length) + bin
    }).mkString

  def decode(input: String): Seq[Packet] = {
    input.foldLeft(("", None: Option[PackageHead], Seq[Packet]())) {
      case ((buffer, openItem, result), nextChar) => openItem match {
        case Some(LiteralPackageHead(version, 4)) if Literal.isValueComplete(buffer + nextChar) =>
          ("", None, result :+ Literal(version, 4, buffer + nextChar))
        case Some(OperatorPackageHead(version, id, lengthTypeId, -1)) if Operator.isLengthComplete(lengthTypeId, buffer + nextChar) =>
          ("", Some(OperatorPackageHead(version, id, lengthTypeId, Integer.parseInt(buffer + nextChar, 2))), result)
        case Some(OperatorPackageHead(version, id, 0, length)) if buffer.length + 1 == length =>
          ("", None, result :+ Operator(version, id, 0, length, buffer + nextChar))
        case Some(OperatorPackageHead(version, id, 1, length)) =>
          val nextDecoded = decode(buffer + nextChar)
          if (nextDecoded.length == length)
            ("", None, result :+ Operator(version, id, 1, length, nextDecoded))
          else
            (buffer + nextChar, openItem, result)
        case Some(_) => (buffer + nextChar, openItem, result)
        case None => buffer match {
          case PACKAGE_HEADER(version, "100") =>
            ("" + nextChar, Some(LiteralPackageHead(Integer.parseInt(version, 2), 4)), result)
          case PACKAGE_HEADER(version, id) =>
            ("", Some(OperatorPackageHead(Integer.parseInt(version, 2), Integer.parseInt(id, 2), nextChar.asDigit, -1)), result)
          case b => (b + nextChar, openItem, result)
        }
      }
    }._3
  }

  @tailrec def versionSum(packets: Seq[Packet], sum: Long = 0): Long = {
    if (packets.isEmpty)
      return sum
    val children = packets.flatMap {
      case Operator(_, _, _, _, children) => children
      case _ => Seq()
    }
    versionSum(children, sum + packets.map(_.version.toLong).sum)
  }

  def versionSum(input: String): Long = versionSum(decode(decodeHex(input)))

  def calcValue(input: String): Long = decode(decodeHex(input)).map(_.value).sum

  def solutionA(input: Seq[String]): Long = versionSum(input.head)

  def solutionB(input: Seq[String]): Long = calcValue(input.head)

  def main(args: Array[String]): Unit = {
    val input = Source.fromResource("day16_input.txt").getLines.toSeq
    println(solutionA(input))
    println(solutionB(input))
  }

}
