

package de.kaibra.aoc2021

import de.kaibra.aoc2021.Day18.{Node, Pair, PairVal}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day18Spec extends AnyFlatSpec with Matchers {


  "Day18" should "parse a pair" in {
    Pair("[1,2]") shouldEqual Pair(1, 2)
    Pair("[[1,2],3]") shouldEqual Pair(Pair(1, 2), 3)
    Pair("[[1,2],[3,4]]") shouldEqual Pair(Pair(1, 2), Pair(3, 4))
    Pair("[[1,[2,3]],[4,5]]") shouldEqual Pair(Pair(1, Pair(2, 3)), Pair(4, 5))
  }

  "Day18" should "add a pair" in {
    Pair(1, 2) + Pair(1, 2) shouldEqual Pair(Pair(1, 2), Pair(1, 2))
    Pair(1, 2) + 1 shouldEqual Pair(Pair(1, 2), 1)
    Pair(1, 2) + 1 + 2 shouldEqual Pair(Pair(Pair(1, 2), 1), 2)
  }

  "Day18" should "add multiple pairs" in {
    Pair("[1,1]") + Pair("[2,2]") + Pair("[3,3]") + Pair("[4,4]") shouldEqual Pair("[[[[1,1],[2,2]],[3,3]],[4,4]]")

    Pair("[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]") +
      Pair("[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]") +
      Pair("[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]") +
      Pair("[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]") +
      Pair("[7,[5,[[3,8],[1,4]]]]") +
      Pair("[[2,[2,2]],[8,[8,1]]]") +
      Pair("[2,9]") +
      Pair("[1,[[[9,3],9],[[9,0],[0,7]]]]") +
      Pair("[[[5,[7,4]],7],1]") +
      Pair("[[[[4,2],2],6],[8,7]]") shouldEqual Pair("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]")
  }

  "Day18" should "calc some magnitudes" in {
    Pair("[[9,1],[1,9]]").magnitude shouldEqual 129
    Pair("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude shouldEqual 3488
    (Pair("[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]") + Pair("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]")).magnitude shouldEqual 3993
  }

  "Day18" should "calc the magnitude of an example homework assignment" in {
    (Pair("[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]") +
      Pair("[[[5,[2,8]],4],[5,[[9,9],0]]]") +
      Pair("[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]") +
      Pair("[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]") +
      Pair("[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]") +
      Pair("[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]") +
      Pair("[[[[5,4],[7,7]],8],[[8,3],8]]") +
      Pair("[[9,3],[[9,9],[6,[4,9]]]]") +
      Pair("[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]") +
      Pair("[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]")).magnitude shouldEqual 4140
  }

  "Day18" should "set up parents for Node Node constructor" in {
    val root = Pair(Pair(1, Pair(2, 3)), Pair(11, 22))
    root.parent shouldEqual None
    root.left.parent shouldEqual Some(root)
    root.right.parent shouldEqual Some(root)

    root.left.asInstanceOf[Pair].left.parent shouldEqual Some(root.left)
    root.left.asInstanceOf[Pair].right.parent shouldEqual Some(root.left)
  }

  "Day18" should "set up parents for Node Int constructor" in {
    val root = Pair(Pair(1, Pair(2, 3)), 1)
    root.parent shouldEqual None
    root.left.parent shouldEqual Some(root)
    root.right.parent shouldEqual Some(root)

    root.left.asInstanceOf[Pair].left.parent shouldEqual Some(root.left)
    root.left.asInstanceOf[Pair].right.parent shouldEqual Some(root.left)
  }

  "Day18" should "should explode some pairs" in {
    Pair("[[[[[9,8],1],2],3],4]")
      .reduce shouldEqual Pair("[[[[0,9],2],3],4]")

    Pair("[7,[6,[5,[4,[3,2]]]]]")
      .reduce shouldEqual Pair("[7,[6,[5,[7,0]]]]")

    Pair("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
      .reduce shouldEqual Pair("[[3,[2,[8,0]]],[9,[5,[7,0]]]]")
  }

  "Day18" should "find reduced results" in {
    Pair("[[[[4,3],4],4],[7,[[8,4],9]]]") + Pair("[1,1]") shouldEqual Pair("[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")
  }

  "Day18" should "apply a single reduction step (explosion)" in {
    val pair = Pair("[[[[[9,8],1],2],3],4]")
    pair.reduceSingleStep
    pair shouldEqual Pair("[[[[0,9],2],3],4]")
  }

  "Day18" should "apply a single reduction step (split)" in {
    val pair_1 = Pair("[[[[10,1],2],3],4]")
    pair_1.reduceSingleStep
    pair_1 shouldEqual Pair("[[[[[5,5],1],2],3],4]")

    val pair_2 = Pair("[[[[11,1],2],3],4]")
    pair_2.reduceSingleStep
    pair_2 shouldEqual Pair("[[[[[5,6],1],2],3],4]")

    val pair_3 = Pair("[[[[1,1],2],3],47]")
    pair_3.reduceSingleStep
    pair_3 shouldEqual Pair("[[[[1,1],2],3],[23,24]]")
  }

  "Day18" should "find first explodable" in {
    Pair("[[[[[9,8],[1,2]],2],3],4]")
      .findExplodable shouldEqual Some(Pair(9, 8))

    Pair("[[6,[5,[4,[3,2]]]],1]")
      .findExplodable shouldEqual Some(Pair(3, 2))

    Pair("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]")
      .findExplodable shouldEqual Some(Pair(7, 3))

    Pair("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
      .findExplodable shouldEqual Some(Pair(3, 2))
  }

  "Day18" should "find first splittable" in {
    Pair("[[[[[99,8],[100,2]],2],3],4]")
      .findSplittable shouldEqual Some(new PairVal(99))

    Pair("[[[[[9,8],[10,2]],2],3],4]")
      .findSplittable shouldEqual Some(new PairVal(10))
  }

  "Day18" should "find no explodable" in {
    Pair("[[[[9,1],2],3],4]")
      .findExplodable shouldEqual None
  }

  "Day18" should "split input" in {
    Node.split("1,2") should equal("1", "2")
    Node.split("[1,[2,8]],8")._1 shouldEqual "[1,[2,8]]"
    Node.split("[1,[2,8]],8")._2 shouldEqual "8"
  }

  "Day18b" should "should calc result" in {
    Day18.solutionB(Seq(
      "[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]",
      "[[[5,[2,8]],4],[5,[[9,9],0]]]",
      "[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]",
      "[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]",
      "[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]",
      "[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]",
      "[[[[5,4],[7,7]],8],[[8,3],8]]",
      "[[9,3],[[9,9],[6,[4,9]]]]",
      "[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]",
      "[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"
    )) shouldEqual 3993
  }

}
