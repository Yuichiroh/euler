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
  val max = Iterator.from(1).dropWhile(m => BigInt(9).pow(n) * m >= BigInt(10).pow(m)).next

  def powers(n: Int) = (0 to 9).map(BigInt(_).pow(n))
  val powersList = powers(n)

  /** greedy way */
  val sw = new Stopwatch
  val list = (2 to BigInt(10).pow(max).toInt).filter(e => e.toString.split("").drop(1).map(e2 => powersList(e2.toInt)).sum == e)
  //  list.foreach(println)
  println("result:" + list.sum)
  println(sw.readSplit)

  /** べき乗数以外はさぼる */
  val sw2 = new Stopwatch
  val format = ("%0" + max + "d")
  val digitsCombinations = (0 to 9).map(List.fill(max)(_)).flatten.combinations(max).drop(2) // (0,...,0)と(0,...,1)を除く
  val sums = for {
    combination <- digitsCombinations
    sum = combination.map(powersList(_)).sum
    newList = format.format(sum).split("").drop(1).map(_.toInt).toVector.sorted
    if combination.equals(newList)
  } yield sum
  println("result:" + sums.sum)
  println(sw2.readSplit)
}