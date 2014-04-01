package nlp.scala.euler

import nlp.scala.util.Stopwatch

object P5 extends App {
  /**
   * Smallest multiple
   * Problem 5
   * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
   *
   * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
   */

  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  def lcm(m: Int, n: Int): Int = m * (n / gcd(m, n))

  def solution1 = (11 to 20).reduceLeft(lcm)

  val sw = new Stopwatch
  args.toSeq match {
    case "1" +: s => sw.time(solution1, "s1")
    case _ =>
  }

}