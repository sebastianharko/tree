package com.harko

import scala.collection.mutable
import scala.io.StdIn._
import scala.util.{Failure, Success, Try}

object TriangleUtils {

  sealed trait Triangle {
    val sum: Int
  }

  case class FinalNode(value: Int, sum: Int) extends Triangle

  case class Node(value: Int, left: Triangle, right: Triangle, sum: Int) extends Triangle

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  def minSum(left: Triangle, right: Triangle): Int = {
    Math.min(left.sum, right.sum)
  }

  def getTreeFromLines(data: Array[Array[Int]]): Triangle = {

    lazy val get: ((Int, Int)) => Triangle = memoize[(Int, Int), Triangle] {
      case (row: Int, col: Int) if row == data.length - 1 => {
        val value = data(row)(col)
        FinalNode(value, sum = value)
      }
      case (row: Int, col: Int) =>
        val value = data(row)(col)
        val left = get(row + 1, col)
        val right = get(row + 1, col + 1)
        Node(value,
          left,
          right,
          sum = value + minSum(left, right))
    }

    get(0, 0)

  }

  def followMinPath(triangle: Triangle): List[Int] = {
    triangle match {
      case FinalNode(value, _) =>
        value :: Nil
      case Node(value, left, right, _) if left.sum <= right.sum =>
        value :: followMinPath(left)
      case Node(value, left, right, _) =>
        value :: followMinPath(right)
    }
  }

}

object Main extends App {

  private def parseLine(line: String): Array[Int] =
    line.split(" ").map(item => item.toInt)

  def parseText(text: String): Try[Array[Array[Int]]] = Try {
    text.split("\n")
      .map(parseLine)
      .toArray
  }

  val text: String = readLine()

  parseText(text) match {
    case Success(data) =>
      println("Minimal path is:")
    case Failure(ex) =>
      println(s"Failed to parse input ${ex.toString}")
  }

}
