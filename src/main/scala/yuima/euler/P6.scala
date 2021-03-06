package yuima.euler

object P6 extends App {
  /**
   * Sum square difference
   * Problem 6
   * The sum of the squares of the first ten natural numbers is,
   *
   * 1^2 + 2^2 + ... + 10^2 = 385
   * The square of the sum of the first ten natural numbers is,
   *
   * (1 + 2 + ... + 10)^2 = 55^2 = 3025
   * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 3025 − 385 = 2640.
   *
   * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
   */

  def solution0 = {
    val sum = (1 to 100).sum
    val squareSum = (1 to 100).map(n => n * n).sum
    sum * sum - squareSum
  }

  println(solution0)
}