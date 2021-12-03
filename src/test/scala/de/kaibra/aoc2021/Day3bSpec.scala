package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day3bSpec extends AnyFlatSpec with Matchers {
  "Day3b" should "find the correct solution" in {
    Day3b.day3SolutionB(Seq(
      "00100",
      "11110",
      "10110",
      "10111",
      "10101",
      "01111",
      "00111",
      "11100",
      "10000",
      "11001",
      "00010",
      "01010"
    )) shouldEqual 230
  }
}
