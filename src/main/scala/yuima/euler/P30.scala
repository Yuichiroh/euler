package yuima.euler

import scala.math.BigInt.int2bigInt

/** Digit fifth powers
  * Problem 30
  *
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
object P30 extends App {

  val n = if (args.size > 1) args(1).toInt else 5
  val sId = if (args.size > 0) args(0).toInt else 1

  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }

  /** greedy way */
  def solution0 = {
    val max = Iterator.from(1).dropWhile(m => 9.pow(n) * m >= 10.pow(m)).next()
    val powers = (0 to 9).map(_.pow(n))
    (2 to 10.pow(max).toInt).filter(num => num.digits.map(powers(_).toInt).sum == num).sum
  }

  /** 乗数の和が同じになる数の計算を省く */
  def solution1 = {
    val powers = (0 to 9).map(_.pow(n))
    val max = Iterator.from(1).dropWhile(m => powers(9) * m >= 10.pow(m)).next()
    val digitsCombinations = (0 to 9).flatMap(List.fill(max)(_)).combinations(max).map(_.dropWhile(_ == 0))

    val sums = for {
      combination <- digitsCombinations
      sum = combination.map(powers(_)).sum
      if sum.toInt.digits.sorted.dropWhile(_ == 0) == combination
    } yield sum

    sums.sum - 1 // 1を除く
  }
  println(solution)
}
