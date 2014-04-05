package euler.scala

object P1 extends App {
  /**
   * Multiples of 3 and 5
   * Problem 1
   * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
   *
   * Find the sum of all the multiples of 3 or 5 below 1000.
   */

  val max = 1000

  def multiples(n: Int) = Iterator.from(1).map(n*).takeWhile(_ < max)

  /** 等差数列の和 */
  def arithmeticSeriesSum(num: Int) = {
    val n = (max - 1) / num
    num * n * (n + 1) / 2
  }

  def solution0 = (1 until max).filter(n => n % 3 == 0 || n % 5 == 0).sum
  def solution1 = (multiples(3) ++ multiples(5)).toList.distinct.sum
  def solution2 = multiples(3).sum + multiples(5).sum - multiples(15).sum
  def solution3 = arithmeticSeriesSum(3) + arithmeticSeriesSum(5) - arithmeticSeriesSum(15)

  val solutions = List(solution0, solution1, solution2, solution3)
  val sId = if (args.size > 0) args(0).toInt else 0
  println(solutions(sId))
}