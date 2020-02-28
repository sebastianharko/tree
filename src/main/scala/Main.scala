import scala.io.StdIn._

sealed trait Triangle

case class FinalNode(value: Int) extends Triangle

case class Node(value: Int, left: Triangle, right: Triangle) extends Triangle

object Triangle {

  def getTreeFromLines(row: Int, col: Int, data: List[List[Int]]): Triangle = {
    row match {
      case last if row == data.size - 1 =>
        val value = data(row)(col)
        FinalNode(value)
      case _ =>
        val value = data(row)(col)
        Node(value, left = getTreeFromLines(row + 1, col, data),
          right = getTreeFromLines(row + 1, col + 1, data))
    }
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

  def parseLine(line: String): List[Int] =
    line.split(" ").toList.map(item => item.toInt)

  def parseText(text: String): List[List[Int]] =
    text.split("\n")
      .map(line => parseLine(line))
      .toList

  val text: String = readLine()

  parseText(text)

}