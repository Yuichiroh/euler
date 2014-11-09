package euler

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
import euler.DirectionalIntegerGraph._

object DirectionalIntegerGraph {
  case class Node(n: Int,
                  from: ArrayBuffer[Int] = ArrayBuffer[Int](),
                  toward: ArrayBuffer[Int] = ArrayBuffer[Int]()) {
    override def toString() = from.mkString("(", ",", ")") + "->" + n + "->" + toward.mkString("(", ",", ")")
  }
}

class DirectionalIntegerGraph {
  val nodes = mutable.Map[Int, Node]()
  override def toString() = nodes.values.map(_.toString()).mkString("\n")
}