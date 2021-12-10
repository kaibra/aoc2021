
package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day9aSpec extends AnyFlatSpec with Matchers {
  "Day9a" should "find the correct solution" in {
    Day9a.day9SolutionA(Seq(
      "2199943210",
      "3987894921",
      "9856789892",
      "8767896789",
      "9899965678"
    )) shouldEqual 15
  }

}
