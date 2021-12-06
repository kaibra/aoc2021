package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day6aSpec extends AnyFlatSpec with Matchers {
  "Day6a" should "find the correct solution" in {
    Day6a.day6SolutionA(Seq(
      "3,4,3,1,2"
    ), 18) shouldEqual 26

    Day6a.day6SolutionA(Seq(
      "3,4,3,1,2"
    ), 80) shouldEqual 5934
  }
}
