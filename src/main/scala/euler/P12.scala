package euler.p1to20

object P12 extends App {
  /**
   * Highly divisible triangular number
   * Problem 12
   * The sequence of triangle numbers is generated by adding the natural numbers.
   * So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
   *
   * 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
   *
   * Let us list the factors of the first seven triangle numbers:
   *
   * 1: 1
   * 3: 1,3
   * 6: 1,2,3,6
   * 10: 1,2,5,10
   * 15: 1,3,5,15
   * 21: 1,3,7,21
   * 28: 1,2,4,7,14,28
   * We can see that 28 is the first triangle number to have over five divisors.
   *
   * What is the value of the first triangle number to have over five hundred divisors?
   */

  def triangleNumbers = Iterator.from(1).map(n => n * (n + 1) / 2)

  def factorize(num: Long, prime: Long = 2L): List[Long] = num match {
    case 1 => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0 => prime :: factorize(num / prime, prime)
    case _ => factorize(num, prime + 1)
  }

  def numDividers(num: Long) = (Map[Long, Int]() /: factorize(num)) { (ds, d) =>
    ds + (d -> (ds.getOrElse(d, 0) + 1))
  }.map(_._2 + 1).product

  def solution0 = triangleNumbers.find(numDividers(_) >= 500).get

  println(solution0)
}