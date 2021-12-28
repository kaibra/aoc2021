

package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day17Spec extends AnyFlatSpec with Matchers {

  "Day17" should "find best" in {
    Day17.solutionA(Seq("target area: x=20..30, y=-10..-5")) shouldEqual 45
  }

  "Day17" should "calc xReihe" in {
    Day17.xReihe(12) shouldEqual(Seq(12, 23, 33, 42, 50, 57, 63, 68, 72, 75, 77, 78), true)
    Day17.xReihe(10) shouldEqual(Seq(10, 19, 27, 34, 40, 45, 49, 52, 54, 55), true)
    Day17.xReihe(6) shouldEqual(Seq(6, 11, 15, 18, 20, 21), true)
    Day17.xReihe(6, 22) shouldEqual(Seq(6, 11, 15, 18, 20, 21), true)
    Day17.xReihe(6, 11) shouldEqual(Seq(6, 11), false)
    Day17.xReihe(6, 20) shouldEqual(Seq(6, 11, 15, 18, 20), false)
  }

  "Day17" should "calc yReihe" in {
    Day17.yReihe(6, min = -5) shouldEqual List(6, 11, 15, 18, 20, 21, 21, 20, 18, 15, 11, 6, 0)
    Day17.yReihe(6, min = -20) shouldEqual List(6, 11, 15, 18, 20, 21, 21, 20, 18, 15, 11, 6, 0, -7, -15)
  }

  "Day17" should "find nr of possible solutions" in {
    Day17.solutionB(Seq("target area: x=20..30, y=-10..-5")) shouldEqual 112
  }

  val EXPECTED_VALS = List(
    (23, -10),
    (25, -9),
    (27, -5),
    (29, -6),
    (22, -6),
    (21, -7),
    (9, 0),
    (27, -7),
    (24, -5),
    (25, -7),
    (26, -6),
    (25, -5),
    (6, 8),
    (11, -2),
    (20, -5),
    (29, -10),
    (6, 3),
    (28, -7),
    (8, 0),
    (30, -6),
    (29, -8),
    (20, -10),
    (6, 7),
    (6, 4),
    (6, 1),
    (14, -4),
    (21, -6),
    (26, -10),
    (7, -1),
    (7, 7),
    (8, -1),
    (21, -9),
    (6, 2),
    (20, -7),
    (30, -10),
    (14, -3),
    (20, -8),
    (13, -2),
    (7, 3),
    (28, -8),
    (29, -9),
    (15, -3),
    (22, -5),
    (26, -8),
    (25, -8),
    (25, -6),
    (15, -4),
    (9, -2),
    (15, -2),
    (12, -2),
    (28, -9),
    (12, -3),
    (24, -6),
    (23, -7),
    (25, -10),
    (7, 8),
    (11, -3),
    (26, -7),
    (7, 1),
    (23, -9),
    (6, 0),
    (22, -10),
    (27, -6),
    (8, 1),
    (22, -8),
    (13, -4),
    (7, 6),
    (28, -6),
    (11, -4),
    (12, -4),
    (26, -9),
    (7, 4),
    (24, -10),
    (23, -8),
    (30, -8),
    (7, 0),
    (9, -1),
    (10, -1),
    (26, -5),
    (22, -9),
    (6, 5),
    (7, 5),
    (23, -6),
    (28, -10),
    (10, -2),
    (11, -1),
    (20, -9),
    (14, -2),
    (29, -7),
    (13, -3),
    (23, -5),
    (24, -8),
    (27, -9),
    (30, -7),
    (28, -5),
    (21, -10),
    (7, 9),
    (6, 6),
    (21, -5),
    (27, -10),
    (7, 2),
    (30, -9),
    (21, -8),
    (22, -7),
    (24, -9),
    (20, -6),
    (6, 9),
    (29, -5),
    (8, -2),
    (27, -8),
    (30, -5),
    (24, -7),
  )
  "Day17" should "find all possible solutions" in {
    val result = Day17.allMatches(Seq("target area: x=20..30, y=-10..-5")).sorted
    result should contain theSameElementsAs EXPECTED_VALS.sorted
  }

}
