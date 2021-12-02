package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1bSpec extends AnyFlatSpec with Matchers {
  "Day1b" should "find the correct solution" in {
    Day1b.day1SolutionB(Seq(
      199,
      200,
      208,
      210,
      200,
      207,
      240,
      269,
      260,
      263
    )) shouldEqual 5
  }
}
