package euler.p1to20


import scala.collection.immutable.Stream.consWrapper

object P14 extends App {
  /**
   * Longest Collatz sequence
   * Problem 14
   *
   * The following iterative sequence is defined for the set of positive integers:
   *
   * n → n/2 (n is even)
   * n → 3n + 1 (n is odd)
   *
   * Using the rule above and starting with 13, we generate the following sequence:
   *
   * 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
   * It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
   *
   * Which starting number, under one million, produces the longest chain?
   *
   * NOTE: Once the chain starts the terms are allowed to go above one million.
   */

  val init = 1000000
  val seqLengths = Array.fill(init)(0)

  def seqLength(num: Long): Int = num match {
    case 1 => 1
    case _ => {
      val next = if (num % 2 == 0) num / 2L else 3L * num + 1L
      if (seqLengths.length > next) seqLengths(next.toInt) = -1
      seqLength(next) + 1
    }
  }

  def solution0 = {
    for (num <- (init - 1 to 1 by -1) if seqLengths(num) >= 0) seqLengths(num) = seqLength(num)
    seqLengths.indexOf(seqLengths.max)
  }

  println(solution0)
}