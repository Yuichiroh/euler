package yuima.euler

/**
 * Power digit sum
 * Problem 16
 *
 * 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
 *
 * What is the sum of the digits of the number 2^1000?
 */
object P16 extends App {

  def solution0 = BigInt(2).pow(1000).toString.map(_.asDigit).sum

  println(solution0)
}