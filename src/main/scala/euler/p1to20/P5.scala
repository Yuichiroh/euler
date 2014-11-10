package euler

object P5 extends App {
  /**
   * Smallest multiple
   * Problem 5
   * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
   *
   * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
   */

  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  def lcm(m: Int, n: Int): Int = m * (n / gcd(m, n))

  def solution0 = (11 to 20).reduceLeft(lcm)

  println(solution0)
}