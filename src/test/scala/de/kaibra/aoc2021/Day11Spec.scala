

package de.kaibra.aoc2021

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day11Spec extends AnyFlatSpec with Matchers {
  val INPUT = Seq(
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526",
  )

  def outputAfterSteps(nrSteps: Int): String = {
    val grid = Grid(INPUT)
    for (_ <- 1 to nrSteps) yield grid.step
    grid.octopuses.map(_.energyLevel).mkString("")
  }

  "Day11a" should "find the correct solution" in {
    Day11.solutionA(INPUT) shouldEqual 1656
  }

  "Day11b" should "find the correct solution" in {
    Day11.solutionB(INPUT) shouldEqual 195
  }

  "Day11a" should "have matrix after step 4" in {
    outputAfterSteps(4) shouldEqual
      """
      2263031977
      0923031697
      0032221150
      0041111163
      0076191174
      0053411122
      0042361120
      5532241122
      1532247211
      1132230211
      """.replaceAll("\\s", "")
  }

  "Day11a" should "have matrix after step 7" in {
    outputAfterSteps(7) shouldEqual
      """
      6707366222
      4377366333
      4475555827
      3496655709
      3500625609
      3509955566
      3486694453
      8865585555
      4865580644
      4465574644
      """.replaceAll("\\s", "")
  }

  "Day11a" should "have matrix after step 40" in {
    outputAfterSteps(40) shouldEqual
      """
      6211111981
      0421111119
      0042111115
      0003111115
      0003111116
      0065611111
      0532351111
      3322234597
      2222222976
      2222222762
      """.replaceAll("\\s", "")
  }

}
