package euler.scala

import scala.collection.immutable.Stream.consWrapper
import scala.math.BigInt.int2bigInt
import scala.language.implicitConversions

object Euler {
  class Fraction private (val numerator: BigInt, val denominator: BigInt) {
    override def toString = {
      if (denominator != 1) new StringBuilder(numerator.toString).append("/").append(denominator.toString).toString
      else numerator.toString
    }

    def *(f: Fraction) = Fraction(numerator * f.numerator, denominator * f.denominator)
    def +(f: Fraction) = Fraction(numerator * f.denominator + denominator * f.numerator, denominator * f.denominator)
    def ==(f: Fraction) = { if (numerator == f.numerator && denominator == f.denominator) true else false }
    implicit def FractionInt(n: BigInt): Fraction = new Fraction(n, 1)
  }

  object Fraction {
    def apply(num: BigInt, den: BigInt) = {
      val d = gcd(num, den)
      new Fraction(num / d, den / d)
    }

    def gcd(m: BigInt, n: BigInt): BigInt = if (n == 0) m else gcd(n, m % n)
  }

  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  def lcm(m: Int, n: Int): Int = m * (n / gcd(m, n))

  def isPalindrome(num: String) = num.reverse == num

  def factorize(num: Long, prime: Long = 2L): List[Long] = num match {
    case 1 => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0 => prime :: factorize(num / prime, prime)
    case _ => factorize(num, prime + 1)
  }

  def count(list: List[Long]) = (Map[Long, Int]().withDefaultValue(0) /: list) { (m, k) => m + (k -> (m(k) + 1)) }

  def triangleNumber(num: Int): Int = if (num == 0) 0 else num + triangleNumber(num - 1)
  def triangleNumbers = Iterator.from(1).map(triangleNumber)

  def isPrime(n: Int) = Iterator.from(2).takeWhile(m => m * m <= n).forall(n % _ != 0)

  def primeNumbers(max: Int) = {
    val primes = Array.fill(max)(true)
    for {
      prime <- Iterator.from(2).takeWhile(n => n * n < max - 1).filter(primes)
      multi <- (prime * 2) to (max - 1) by prime
    } primes(multi) = false
    (2 to (max - 1)).filter(primes(_))
  }

  def primeNumbersArray(index: Int) = {
    val primes = new Array[Int](index)
    primes(0) = 2
    for (i <- (1 to index - 1)) {
      primes(i) = Iterator.from(primes(i - 1) + 1).withFilter(m =>
        (0 to i - 1).forall(m % primes(_) != 0)).next
    }
    primes
  }

  //  def fib(a: Int = 1, b: Int = 1): Stream[Int] = a #:: fib(b, a + b)
  def fib(a: BigInt = 1, b: BigInt = 1): Stream[BigInt] = a #:: fib(b, a + b)

  def sumOfDivisers(num: Int) = factorize(num).groupBy(prime => prime).map(e => (0 to e._2.length).map(k => BigInt(e._1).pow(k)).sum.toInt).product - num

  //  implicit class RichInt(val n: Int) {
  //    def ! : BigInt = (BigInt(1) to n).product
  //  }
}