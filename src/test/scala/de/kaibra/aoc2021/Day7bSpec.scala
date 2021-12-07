
package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day7bSpec extends AnyFlatSpec with Matchers {
  "Day7b" should "find the correct solution" in {
    Day7b.day7SolutionB(Seq(
      "16,1,2,0,4,2,7,1,2,14"
    )) shouldEqual 168
  }
}
