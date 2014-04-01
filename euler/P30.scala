package nlp.scala.euler

import nlp.scala.util.Stopwatch

object P30 extends App {
  /**
   * Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:
   *
   * 1634 = 1^4 + 6^4 + 3^4 + 4^4
   * 8208 = 8^4 + 2^4 + 0^4 + 8^4
   * 9474 = 9^4 + 4^4 + 7^4 + 4^4
   * As 1 = 1^4 is not a sum it is not included.
   *
   * The sum of these numbers is 1634 + 8208 + 9474 = 19316.
   *
   * Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.
   */

  val n = args(0).toInt
  def powers(n: Int) = (0 to 9).map(BigInt(_).pow(n))

  /** greedy way */
  def solution1 = {
    val max = Iterator.from(1).dropWhile(m => BigInt(9).pow(n) * m >= BigInt(10).pow(m)).next
    val powersList = powers(n)
    (2 to BigInt(10).pow(max).toInt).filter(e => e.toString.split("").drop(1).map(e2 => powersList(e2.toInt)).sum == e).sum
  }

  /** 乗数の和が同じになる数の計算を省く */
  def solution2 = {
    val max = Iterator.from(1).dropWhile(m => BigInt(9).pow(n) * m >= BigInt(10).pow(m)).next
    val powersList = powers(n)
    val format = ("%0" + max + "d")
    val digitsCombinations = (0 to 9).map(List.fill(max)(_)).flatten.combinations(max)
    val sums = for {
      combination <- digitsCombinations
      sum = combination.map(powersList(_)).sum
      newList = format.format(sum).split("").drop(1).map(_.toInt).toVector.sorted
      //      newList = format.format(sum).toCharArray.map(_.toString.toInt).toVector.sorted
      if combination.equals(newList)
    } yield sum
    sums.sum - 1 // 1を除く
  }

  val sw = new Stopwatch
  args(1) match {
    case "1" => sw.time(solution1, "s1")
    case "2" => sw.time(solution2, "s2")
    case _ =>
  }
}