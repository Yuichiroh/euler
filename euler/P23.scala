package euler.scala

import scala.math.BigInt.int2bigInt

object P23 extends App {
  /**
   * A perfect number is a number for which the sum of its proper divisors is exactly equal to the number.
   * For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
   *
   * A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
   *
   * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24.
   * By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers.
   * However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
   *
   * Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
   */

  def factorize(num: Long, prime: Long = 2L): List[Long] = num match {
    case 1 => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0 => prime :: factorize(num / prime, prime)
    case _ => factorize(num, prime + 1)
  }

  def sumOfDivisers(num: Int) = factorize(num).groupBy(prime => prime).map(e => (0 to e._2.length).map(k => BigInt(e._1).pow(k)).sum.toInt).product - num

  // 28123より小さいabundant numberを全列挙
  // ２つのabundant numberの和を全列挙
  // (1 to 28123).sum - (sum of two abundant numbers).sum

  val abundernts = (12 to 28123).filter(num => num < sumOfDivisers(num))
  val sumsOfAbunderntPair = for (n <- abundernts; m <- abundernts; sum = n + m if sum <= 28123) yield (n + m)
  
  def solution0 = BigInt(28123) * 28124 / 2 - sumsOfAbunderntPair.distinct.sum

  println(solution0)
}