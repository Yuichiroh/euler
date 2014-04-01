package nlp.scala.euler

import nlp.scala.util.Stopwatch

object P7 extends App {
  /**
   * 10001st prime
   * Problem 7
   * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
   *
   * What is the 10 001st prime number?
   */

  def primes: Stream[Int] = 2 #:: Stream.from(3).filter(num => primes.takeWhile(prime => prime * prime <= num).forall(num % _ > 0))
  def solution1 = primes(10000)

  val sw = new Stopwatch
  args.toSeq match {
    case "1" +: s => sw.time(solution1, "s1")
    case _ =>
  }
}