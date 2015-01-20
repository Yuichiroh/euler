package yuima

import scala.annotation.tailrec

package object euler {
  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  def lcm(m: Int, n: Int): Int = m * (n / gcd(m, n))

  def isPalindrome(num: String) = num.reverse == num

  def factorize(num: Long, prime: Long = 2L): List[Long] = num match {
    case 1                        => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0    => prime :: factorize(num / prime, prime)
    case _                        => factorize(num, prime + 1)
  }

  def digits(n: Int): List[Int] = n match {
    case 0 => Nil
    case _ => (n % 10) :: digits(n / 10)
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

  def isPrimeMillerRabin(n: Long) = {
    def divBy2(n: Long): Long = if (n % 2 != 0) n else divBy2(n / 2)

    def pow(base: Long, power: Long, mod: Long, result: Long): Long = {
      if (power > 0) pow((base * base) % mod, power >> 1, mod, if ((power & 1) == 1) (result * base) % mod else result)
      else result
    }

    if (n == 2) true
    else if (n % 2 == 0) false
    else if (n < 9) true
    else if (n % 3 == 0) false
    else {
      val d = divBy2(n - 1)
      val pseudoPrimes = Set(25326001L, 161304001L, 960946321L, 1157839381L, 3215031751L, 3697278427L, 5764643587L, 6770862367L, 14386156093L, 15579919981L, 18459366157L, 19887974881L, 21276028621L)
      !pseudoPrimes.contains(n) && Seq(2, 3, 5).forall { a =>
        var t = d
        var y = pow(a, t, n, 1L)
        while (t != n - 1 && y != 1 && y != n - 1) {
          y = (y * y) % n
          t <<= 1
        }
        y == n - 1 || (t & 1) != 0
      }
    }
  }

  //  def fib(a: Int = 1, b: Int = 1): Stream[Int] = a #:: fib(b, a + b)
  def fib(a: BigInt = 1, b: BigInt = 1): Stream[BigInt] = a #:: fib(b, a + b)

  def sumOfDivisers(num: Int) = factorize(num).groupBy(prime => prime).map(e => (0 to e._2.length).map(k => BigInt(e._1).pow(k)).sum.toInt).product - num

  /**
   * see Wikipedia article: Methods of computing square roots
   * http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
   */
  @tailrec
  def continuedFractionOfRoot(n: Int, a0: Int, as: List[Int], d: Int, m: Int): List[Int] = {
    val newD = m * as.head - d
    val newM = (n - newD * newD) / m
    val newA = ((a0 + newD) / newM).toInt
    if (newA == a0 * 2) newA :: as
    else continuedFractionOfRoot(n, a0, newA :: as, newD, newM)
  }

  class Fraction private(val numerator: BigInt, val denominator: BigInt) {
    override def toString = {
      if (denominator != 1) new StringBuilder(numerator.toString).append("/").append(denominator.toString).toString
      else numerator.toString
    }

    def *(f: Fraction) = Fraction(numerator * f.numerator, denominator * f.denominator)

    def /(f: Fraction) = Fraction(numerator * f.denominator, denominator * f.numerator)

    def +(f: Fraction) = Fraction(numerator * f.denominator + denominator * f.numerator, denominator * f.denominator)

    def -(f: Fraction) = Fraction(numerator * f.denominator - denominator * f.numerator, denominator * f.denominator)

    def ==(f: Fraction) = {
      if (numerator == f.numerator && denominator == f.denominator) true else false
    }

    implicit def bigInt2fractionInt(n: BigInt): Fraction = new Fraction(n, 1)

    implicit def int2fraction(n: BigInt): Fraction = new Fraction(n, 1)
  }

  implicit class RichInt(val n: Int) {
    def ! : BigInt = (BigInt(1) to n).product

    def digits: List[Int] = n match {
      case 0 => Nil
      case _ => (n % 10) :: (n / 10).digits
    }
  }

  object Fraction {
    val * = (f1: Fraction) => (f2: Fraction) => Fraction(f1.numerator * f2.numerator, f1.denominator * f2.denominator)
    val / = (f1: Fraction) => (f2: Fraction) => Fraction(f1.numerator * f2.denominator, f1.denominator * f2.numerator)
    val + = (f1: Fraction) => (f2: Fraction) => Fraction(f1.numerator * f2.denominator + f1.denominator * f2.numerator, f1.denominator * f2.denominator)
    val - = (f1: Fraction) => (f2: Fraction) => Fraction(f1.numerator * f2.denominator - f1.denominator * f2.numerator, f1.denominator * f2.denominator)

    def apply(num: BigInt, den: BigInt) = {
      require(den != 0)
      val d = gcd(num, den)
      if (den > 0) new Fraction(num / d, den / d)
      else new Fraction(-1 * num / d, -1 * den / d)
    }

    def apply(num: BigInt) = {
      new Fraction(num, 1)
    }

    def gcd(m: BigInt, n: BigInt): BigInt = if (n == 0) m else gcd(n, m % n)
  }

}