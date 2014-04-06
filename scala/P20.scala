package euler.scala

import scala.math.BigInt.int2bigInt
import scala.language.postfixOps

object P20 extends App {
  /**
   * n! means n × (n − 1) × ... × 3 × 2 × 1
   *
   * For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
   * and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
   *
   * Find the sum of the digits in the number 100!
   */
  implicit class RichInt(val n: Int) {
    def ! : BigInt = (BigInt(1) to n).product
  }

  println((100!).toString.map(_.asDigit).sum)
}