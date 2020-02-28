package com.harko

import com.harko.Main.parseText
import com.harko.TriangleUtils.{FinalNode, Node, Triangle, followMinPath, getTreeFromLines}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Matchers, TryValues}

import scala.io.Source
import scala.util.Success

class TriangleTest extends AnyFunSuite with Matchers with TryValues {

  test("Can parse text lines") {

    val input =
      """|1
         |1 2
         |1 2 3
         |""".stripMargin

    parseText(input).map(_.toList.map(_.toList)) should be(
      Success(List(
        List(1),
        List(1, 2),
        List(1, 2, 3))))

  }

  test("Can detect errors in input text lines") {

    val inputWithErrors =
      """|1
         |1 a
         |1 2 3
         |""".stripMargin

    parseText(inputWithErrors) shouldBe 'failure

  }

  test("Can build a tree from input text lines") {
    val input: Array[Array[Int]] =  Array(
      Array(1),
      Array(1, 2),
      Array(1, 2, 3))

    val tree: Triangle = getTreeFromLines(input)
    val expected = Node(1,
      Node(1, FinalNode(1, sum = 1), FinalNode(2, sum = 2), sum = 2),
      Node(2, FinalNode(2, sum = 2), FinalNode(3, sum = 3), sum = 4),
      sum = 3)

    tree shouldBe expected

  }

  test("Can find minimum path") {

    val inputFromExample =
      """|7
         |6 3
         |3 8 5
         |11 2 10 9
         |""".stripMargin

    val parsed = parseText(inputFromExample)

    parsed shouldBe 'success

    val tree = getTreeFromLines(parsed.get)

    val minPath = followMinPath(tree)

    minPath.sum shouldBe 18

  }

  test("Can handle file with 500 rows") {
    val f = Source.fromResource("data.txt").mkString
    val parsed = parseText(f)
    parsed shouldBe 'success
    val tree = getTreeFromLines(parsed.get)
    println(followMinPath(tree))

  }

}
