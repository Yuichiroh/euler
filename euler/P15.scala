package nlp.scala.euler

import nlp.scala.util.Stopwatch

object P15 extends App {
  /**
   * Lattice paths
   * Problem 15
   *
   * Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
   * there are exactly 6 routes to the bottom right corner.
   * How many such routes are there through a 20×20 grid?
   */

  /**
   * ゴールに行き着くまで、下２０回、右２０回、計４０回動く。４０個の移動系列のうち、２０個を下移動で埋める。
   * 系列のインデックスが1から40まである。このうち20個を取り出し、下に割り当てるので、40C20。
   * ただし、分母、分子がLongに入りきらないので注意。
   */
  def combination(a: Int, b: Int) = (BigInt(a - b + 1) to a).product / (BigInt(1) to b).product
  def solution1 = combination(40, 20)

  val sw = new Stopwatch
  args.toSeq match {
    case "1" +: s => sw.time(solution1, "s1")
    case _ =>
  }

}