package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day6bSpec extends AnyFlatSpec with Matchers {
  "Day6b" should "find the correct solution" in {
    Day6b.day6SolutionB(Seq(
      "3,4,3,1,2"
    ), 256) shouldEqual BigInt("26984457539")
  }
}
