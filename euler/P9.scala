package nlp.scala.euler

import nlp.scala.util.Stopwatch

object P9 extends App {
  /**
   * Special Pythagorean triplet
   * Problem 9
   * A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
   *
   * a2 + b2 = c2
   * For example, 32 + 42 = 9 + 16 = 25 = 52.
   *
   * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
   * Find the product abc.
   */

  val sum = 1000
  def solution1 = for (a <- 1 to sum / 3; b <- a + 1 to sum / 2; c = sum - a - b if a * a + b * b == c * c) yield a * b * c

  val sw = new Stopwatch
  sw.start("solution1")
  solution1.foreach(println)
  sw.stop
}