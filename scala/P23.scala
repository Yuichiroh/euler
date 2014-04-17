package euler.scala

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

  def factorize(n: Int, prime: Int = 2): List[Int] = n match {
    case 1 => Nil
    case _ if prime * prime > n => n :: Nil
    case _ if n % prime == 0 => prime :: factorize(n / prime, prime)
    case _ => factorize(n, prime + 1)
  }

  //  def sumOfDivisers(n: Int) = factorize(n).groupBy(identity).map(e => (0 to e._2.length).map(k => BigInt(e._1).pow(k)).sum.toInt).product - n
  def sumOfDivisers(n: Int) = (Map[Int, Int]() /: factorize(n)) { (ds, d) => ds + (d -> (ds.getOrElse(d, 0) + 1)) }.map(e => (0 to e._2).map(k => Math.pow(e._1, k)).sum).product - n

  // 28123より小さいabundant numberを全列挙
  // ２つのabundant numberの和を全列挙
  // (1 to 28123).sum - (sum of two abundant numbers).sum

  //  def primeNumbers(max: Int) = {
  //    val primes = Array.fill(max)(true)
  //    for {
  //      prime <- Iterator.from(2).takeWhile(n => n * n < max - 1).filter(primes)
  //      multi <- (prime * 2) to (max - 1) by prime
  //    } primes(multi) = false
  //    primes
  //  }

  //  val isPrime = primeNumbers(28124)
  //  val abundernts = (12 to 28123).filter(n => !isPrime(n) && n < sumOfDivisers(n))
  //  val abundernts = (12 to 28123).filter(num => num % 6 == 0 || num < sumOfDivisers(num))
  val abundernts = (12 to 28123).filter(n => n < sumOfDivisers(n))

  def solution0 = {
    val sumsOfAbunderntPair = for (n <- abundernts; m <- abundernts if n + m <= 28123) yield (n + m)
    28123L * 14062 - sumsOfAbunderntPair.distinct.sum
  }

  def solution1 = {
    val canBeWritten = Array.fill(28124) { false }
    for (n <- abundernts; m <- abundernts if n + m <= 28123) canBeWritten(n + m) = true
    (1 to 28123).filter(!canBeWritten(_)).sum
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}