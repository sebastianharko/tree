package com.harko

import com.harko.Main.{isValidTriangle, parseInput}
import com.harko.TriangleUtils.{FinalNode, Node, Triangle, followMinPath, getTriangleFromLines}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Matchers, TryValues}

import scala.io.Source
import scala.util.Success

class TriangleTest extends AnyFunSuite with Matchers with TryValues {

  test("Can parse triangle from text lines into array") {

    val input =
      """|1
         |1 2
         |1 2 3
         |""".stripMargin.split("\n").toList

    parseInput(input).map(_.toList.map(_.toList)) should be(
      Success(List(
        List(1),
        List(1, 2),
        List(1, 2, 3))))

  }

  test("Fails on bad input") {

    val inputWithErrors =
      """|1
         |1 a
         |1 2 3
         |""".stripMargin.split("\n").toList

    parseInput(inputWithErrors) shouldBe 'failure

  }

  test("Can build a tree from the input text lines") {
    val input: Array[Array[Int]] =  Array(
      Array(1),
      Array(1, 2),
      Array(1, 2, 3))

    val tree: Triangle = getTriangleFromLines(input)
    val expected = Node(1,
      Node(1, FinalNode(1, sum = 1), FinalNode(2, sum = 2), sum = 2),
      Node(2, FinalNode(2, sum = 2), FinalNode(3, sum = 3), sum = 4),
      sum = 3)

    tree shouldBe expected

  }

  test("Can flag invalid tree") {

    isValidTriangle(Array(
      Array(1),
      Array(1, 2),
      Array(1, 2, 4, 5)
    )) shouldBe false

    isValidTriangle(Array(
      Array(1),
      Array(1, 2),
      Array(1, 2, 3)
    )) shouldBe true
  }

  test("Can find minimum path (1)") {

    val inputFromExample =
      """|7
         |6 3
         |3 8 5
         |11 2 10 9
         |""".stripMargin.split("\n").toList

    val parsed = parseInput(inputFromExample)

    parsed shouldBe 'success

    val tree = getTriangleFromLines(parsed.get)

    val minPath = followMinPath(tree)

    minPath.sum shouldBe 18

    minPath shouldBe (List(7, 6, 3, 2))

  }

  test("Can find minimum path (2)") {

    val inputFromExample =
      """|1
         |1 2
         |1 2 3
         |""".stripMargin.split("\n").toList

    val parsed = parseInput(inputFromExample)

    parsed shouldBe 'success

    val tree = getTriangleFromLines(parsed.get)

    val minPath = followMinPath(tree)

    minPath.sum shouldBe 3

    minPath shouldBe (List(1, 1, 1))
  }

  test("Can handle file with 500 rows") {
    val f = Source.fromResource("data.txt").mkString.split("\n").toList
    val parsed = parseInput(f)
    parsed shouldBe 'success
    val tree = getTriangleFromLines(parsed.get)
    followMinPath(tree).sum shouldBe tree.sum

  }

}
