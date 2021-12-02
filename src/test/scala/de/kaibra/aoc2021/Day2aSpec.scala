package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day2aSpec extends AnyFlatSpec with Matchers {
  "Day2a" should "find the correct solution" in {
    Day2a.day2SolutionA(Seq(
   "forward 5",
   "down 5",
   "forward 8",
   "up 3",
   "down 8",
   "forward 2"
    )) shouldEqual 150
  }
}
