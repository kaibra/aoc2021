
package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day7aSpec extends AnyFlatSpec with Matchers {
  "Day7a" should "find the correct solution" in {
    Day7a.day7SolutionA(Seq(
      "16,1,2,0,4,2,7,1,2,14"
    )) shouldEqual 37
  }
}
