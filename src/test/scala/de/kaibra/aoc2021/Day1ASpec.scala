package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day1ASpec extends AnyFlatSpec with Matchers {

  "Day1a" should "find the correct solution" in {
    Day1a.day1SolutionA(Seq(
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
    )) shouldEqual 7
  }
}
