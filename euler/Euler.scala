package nlp.scala.euler

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

  def factorize(num: Long, prime: Long = 2L): List[Long] = num match {
    case 1 => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0 => prime :: factorize(num / prime, prime)
    case _ => factorize(num, prime + 1)
  }

  def count(list: List[Long]) = (Map[Long, Int]().withDefaultValue(0) /: list) { (m, k) => m + (k -> (m(k) + 1)) }

  def triangleNumber(num: Int): Int = if (num == 0) 0 else num + triangleNumber(num - 1)
  def triangleNumbers = Iterator.from(1).map(triangleNumber)

  def primeNumbers(n: Int) = {
    val primes = Array.fill(n)(true)
    for {
      prime <- 2 to (n - 1)
      if primes(prime)
      multi <- (prime * 2) to (n - 1) by prime
    } {
      primes(multi) = false
    }

    (2 to (n - 1)).filter(primes(_))
  }

  //  def fib(a: Int = 1, b: Int = 1): Stream[Int] = a #:: fib(b, a + b)
  def fib(a: BigInt = 1, b: BigInt = 1): Stream[BigInt] = a #:: fib(b, a + b)

  def sumOfDivisers(num: Int) = factorize(num).groupBy(prime => prime).map(e => (0 to e._2.length).map(k => BigInt(e._1).pow(k)).sum.toInt).product - num

  //  implicit class RichInt(val n: Int) {
  //    def ! : BigInt = (BigInt(1) to n).product
  //  }
}