package com.harko

import com.harko.TriangleUtils.getTreeFromLines

import scala.collection.mutable
import scala.io.StdIn._
import scala.util.{Failure, Random, Success, Try}

object TriangleUtils {

  sealed trait Triangle

  case class FinalNode(value: Int) extends Triangle

  case class Node(value: Int, left: Triangle, right: Triangle) extends Triangle

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {
    override def apply(key: I) = getOrElseUpdate(key, f(key))
  }

  def getTreeFromLines(data: Array[Array[Int]]): Triangle = {

    lazy val get: ((Int, Int)) => Triangle = memoize[(Int, Int), Triangle] {
      case (row: Int, col: Int) if row == data.length -1 => {
        val value = data(row)(col)
        FinalNode(value)
      }
      case (row: Int, col: Int) =>
        val value = data(row)(col)
        Node(value, left = get(row + 1, col), right = get(row + 1, col + 1))
    }

    get(0, 0)

  }


  def chooseMin(p: List[Int], q: List[Int]): List[Int] = {
    if (p.sum <= q.sum) p else q
  }

  def getMinPath(triangle: Triangle): List[Int] = {
    triangle match {
      case FinalNode(value) =>
        value :: Nil
      case Node(value, left, right) =>
        value :: chooseMin(getMinPath(left), getMinPath(right))
    }
  }

}

object Main extends App {

  def parseLine(line: String): Array[Int] =
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
    case Failure(_) =>
      println("Failed to parse input")
  }

}
