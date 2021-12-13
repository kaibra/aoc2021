

package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day12Spec extends AnyFlatSpec with Matchers {

  private val EXAMPLE_1 = Seq(
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end",
  )

  private val EXAMPLE_2 = Seq(
    "dc-end",
    "HN-start",
    "start-kj",
    "dc-start",
    "dc-HN",
    "LN-dc",
    "HN-end",
    "kj-sa",
    "kj-HN",
    "kj-dc",
  )

  private val EXAMPLE_3 = Seq(
    "fs-end",
    "he-DX",
    "fs-he",
    "start-DX",
    "pj-DX",
    "end-zg",
    "zg-sl",
    "zg-pj",
    "pj-he",
    "RW-he",
    "fs-DX",
    "pj-RW",
    "zg-RW",
    "start-pj",
    "he-WI",
    "zg-he",
    "pj-fs",
    "start-RW",
  )

  "Day12a" should "find the correct solution" in {
    Day12.solutionA(EXAMPLE_1) shouldEqual 10
  }

  "Day12a" should "find the correct solution for a more complex example" in {
    Day12.solutionA(EXAMPLE_2) shouldEqual 19
  }

  "Day12a" should "find the correct solution for an even more complex example" in {
    Day12.solutionA(EXAMPLE_3) shouldEqual 226
  }

  "Day12b" should "find the correct solution" in {
    Day12.solutionB(EXAMPLE_1) shouldEqual 36
  }

  "Day12b" should "find the correct solution for a more complex example" in {
    Day12.solutionB(EXAMPLE_2) shouldEqual 103
  }

  "Day12b" should "find the correct solution for an even more complex example" in {
    Day12.solutionB(EXAMPLE_3) shouldEqual 3509
  }


}
