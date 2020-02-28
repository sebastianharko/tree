package com.harko

import com.harko.TriangleUtils.{FinalNode, Node}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.{Matchers, TryValues}

import scala.util.Success

class Triangle extends AnyFunSuite with Matchers with TryValues {

  test("Can parse text lines") {

    val input =
      """|1
         |1 2
         |1 2 3
         |""".stripMargin

    Main.parseText(input) should be(Success(
      List(List(1), List(1, 2), List(1, 2, 3))))

  }

  test("Can detect errors in input text lines") {

    val inputWithErrors =
      """|1
         |1 a
         |1 2 3
         |""".stripMargin

    Main.parseText(inputWithErrors) shouldBe 'failure

  }

  test("Can build a tree from input text lines") {
    val input: List[List[Int]] =  List(
      List(1),
      List(1, 2),
      List(1, 2, 3))

    val tree: TriangleUtils.Triangle = TriangleUtils.getTreeFromLines(0, 0, input)
    val expected = Node(1,
      Node(1, FinalNode(1), FinalNode(2)),
      Node(2, FinalNode(2), FinalNode(3)))

    tree shouldBe expected

  }


}
