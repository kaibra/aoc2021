

package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day13Spec extends AnyFlatSpec with Matchers {

  private val EXAMPLE_1 = Seq(
    "6,10",
    "0,14",
    "9,10",
    "0,3",
    "10,4",
    "4,11",
    "6,0",
    "6,12",
    "4,1",
    "0,13",
    "10,12",
    "3,4",
    "3,0",
    "8,4",
    "1,10",
    "2,14",
    "8,10",
    "9,0",
    "",
    "fold along y=7",
    "fold along x=5",
  )
  "Day13a" should "find the correct solution" in {
    Day13.solutionA(EXAMPLE_1) shouldEqual 17
  }

  "Day13b" should "find the correct solution" in {
    Day13.solutionB(EXAMPLE_1) shouldEqual
      """#####
        |#...#
        |#...#
        |#...#
        |#####""".stripMargin
  }


  "Day13b" should "find the correct solution after no fold" in {
    val (coordinates, _) = Day13.parse(EXAMPLE_1)
    Day13.toReadableOutput(coordinates) shouldEqual
      """...#..#..#.
        |....#......
        |...........
        |#..........
        |...#....#.#
        |...........
        |...........
        |...........
        |...........
        |...........
        |.#....#.##.
        |....#......
        |......#...#
        |#..........
        |#.#........""".stripMargin
  }

}
