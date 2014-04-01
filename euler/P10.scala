package nlp.scala.euler

import nlp.scala.util.Stopwatch

object P10 extends App {
  /**
   * Summation of primes
   * Problem 10
   * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
   *
   * Find the sum of all the primes below two million.
   */

  def primeNumbers(n: Int) = {
    val primes = Array.fill(n)(true)
    for {
      prime <- 2 to (n - 1)
      if primes(prime)
      multiple <- (prime * 2) to (n - 1) by prime
    } {
      primes(multiple) = false
    }

    (2 to (n - 1)).filter(primes(_))
  }

  def solution1 = primeNumbers(2000000).map(_.toLong).sum

  val sw = new Stopwatch
  args.toSeq match {
    case "1" +: s => sw.time(solution1, "s1")
    case _ =>
  }

}