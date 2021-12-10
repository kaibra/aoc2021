
package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day9bSpec extends AnyFlatSpec with Matchers {
  "Day9b" should "find the correct solution" in {
    Day9b.day9SolutionB(Seq(
      "2199943210",
      "3987894921",
      "9856789892",
      "8767896789",
      "9899965678"
    )) shouldEqual 1134
  }

}
