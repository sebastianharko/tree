package com.harko

import scala.collection.mutable
import scala.io.StdIn._
import scala.util.{Failure, Success, Try}

object TriangleUtils {

  sealed trait Triangle {
    val minSum: Int
  }

  case class FinalNode(value: Int, minSum: Int)
    extends Triangle

  case class Node(value: Int, left: Triangle, right: Triangle, minSum: Int)
    extends Triangle

  def withMemoization[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I): O = getOrElseUpdate(key, f(key))
  }

  def minSum(left: Triangle, right: Triangle): Int = {
    Math.min(left.minSum, right.minSum)
  }

  def getTriangleFromLines(data: Array[Array[Int]]): Triangle = {

    lazy val get: ((Int, Int)) => Triangle =
      withMemoization[(Int, Int), Triangle] {
        case (row: Int, col: Int) if row == data.length - 1 =>
          val value = data(row)(col)
          FinalNode(value, minSum = value)

        case (row: Int, col: Int) =>
          val value = data(row)(col)
          val left = get(row + 1, col)
          val right = get(row + 1, col + 1)
          Node(value,
            left,
            right,
            minSum = value + minSum(left, right)) // minimal sum is pre-computed
      }

    get(0, 0)

  }

  // minimal sums are pre-computed so we just have to follow the minimum values
  // down the tree
  def followMinPath(triangle: Triangle): List[Int] = {
    triangle match {
      case FinalNode(value, _) =>
        value :: Nil
      case Node(value, left, right, _) if left.minSum <= right.minSum =>
        value :: followMinPath(left)
      case Node(value, _, right, _) =>
        value :: followMinPath(right)
    }
  }

}

object Main extends App {

  import TriangleUtils._

  // Parsing

  private def parseLine(line: String): Array[Int] =
    line.split(" ").map(item => item.toInt)

  def isValidTriangle(data: Array[Array[Int]]): Boolean = {
    data.map(_.length).toList == (1 to data.length).toList
  }

  def parseInput(lines: List[String]): Try[Array[Array[Int]]] = Try {
    lines.toArray.map(parseLine)
  }.flatMap {
    case data if isValidTriangle(data) => Success(data)
    case _ => Failure(new Exception("Not a valid triangle"))
  }

  // Read input
  val lines = (for (ln <- io.Source.stdin.getLines)
    yield ln).toList

  // Result
  parseInput(lines) match {
    case Success(data: Array[Array[Int]]) =>
      val triangle: Triangle = getTriangleFromLines(data)
      val path: List[Int] = followMinPath(triangle)
      println(s"Minimal path is: ${path.mkString(" + ")} = " + path.sum)
    case Failure(ex) =>
      println(s"Failed to parse input: ${ex.getMessage}")
      ex.printStackTrace()
  }

}
