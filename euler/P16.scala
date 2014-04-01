package nlp.scala.euler

import nlp.scala.util.Stopwatch

object P16 extends App {
  /**
   * Power digit sum
   * Problem 16
   *
   * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
   *
   * What is the sum of the digits of the number 2^1000?
   */

  def solution1 = BigInt(2).pow(1000).toString.map(_.asDigit).sum

  val sw = new Stopwatch
  args.toSeq match {
    case "1" +: s => sw.time(solution1, "s1")
    case _ =>
  }
}