

package de.kaibra.aoc2021

import de.kaibra.aoc2021.Day16.{Literal, Operator}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day16Spec extends AnyFlatSpec with Matchers {

  "Day16a" should "decode from hex to binary" in {
    Day16.decodeHex("D2FE28") shouldEqual "110100101111111000101000"
  }

  "Day16a" should "should decode literal" in {
    Day16.decode("110100101111111000101000") shouldEqual Seq(Literal(6, 4, 2021))
  }

  "Day16a" should "should decode multiple literals" in {
    Day16.decode("1101000101001010010001001000000000") shouldEqual Seq(
      Literal(6, 4, 10), Literal(2, 4, 20)
    )
  }

  "Day16a" should "should decode even more multiple literals" in {
    Day16.decode("110100010100101001000100100") shouldEqual Seq(
      Literal(6, 4, 10), Literal(2, 4, 20)
    )
  }

  "Day16a" should "should decode operator" in {
    Day16.decode("00111000000000000110111101000101001010010001001000000000") shouldEqual
      List(Operator(1, 6, 0, 27, List(Literal(6, 4, 10), Literal(2, 4, 20))))
  }

  "Day16b" should "should calc some values" in {
    Day16.calcValue("C200B40A82") shouldEqual 3
    Day16.calcValue("04005AC33890") shouldEqual 54
    Day16.calcValue("880086C3E88112") shouldEqual 7
    Day16.calcValue("CE00C43D881120") shouldEqual 9
    Day16.calcValue("D8005AC2A8F0") shouldEqual 1
    Day16.calcValue("F600BC2D8F") shouldEqual 0
    Day16.calcValue("9C005AC2F8F0") shouldEqual 0
    Day16.calcValue("9C0141080250320F1802104A08") shouldEqual 1
  }


}
