package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day5bSpec extends AnyFlatSpec with Matchers {
  "Day5b" should "find the correct solution" in {
    Day5b.day5SolutionB(Seq(
      "0,9 -> 5,9",
      "8,0 -> 0,8",
      "9,4 -> 3,4",
      "2,2 -> 2,1",
      "7,0 -> 7,4",
      "6,4 -> 2,0",
      "0,9 -> 2,9",
      "3,4 -> 1,4",
      "0,0 -> 8,8",
      "5,5 -> 8,2"
    )) shouldEqual 12
  }
}
