package euler.scala

object P33 extends App {
  /**
   * Digit canceling fractions
   * Problem 33
   * The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.
   * We shall consider fractions like, 30/50 = 3/5, to be trivial examples.
   *
   * There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.
   * If the product of these four fractions is given in its lowest common terms, find the value of the denominator.
   */

  class Fraction private (val numerator: Int, val denominator: Int) {
    def *(f: Fraction) = Fraction(numerator * f.numerator, denominator * f.denominator)
    implicit def FractionInt(n: Int): Fraction = new Fraction(n, 1)

    def ==(f: Fraction) = { if (numerator == f.numerator && denominator == f.denominator) true else false }
  }

  object Fraction {
    def apply(num: Int, den: Int) = {
      val d = gcd(num, den)
      new Fraction(num / d, den / d)
    }

    def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)
  }

  def solution0 = {
    val nonTrivials = for {
      n <- (1 to 9)
      m <- (1 to 9)
      l <- (1 to 9)
      if n != l
      nm = 10 * n + m
      ml = 10 * m + l
      simplified = Fraction(n, l)
      if simplified == Fraction(nm, ml)
    } yield if (n < l) simplified else Fraction(l, n)

    (Fraction(1, 1) /: nonTrivials) { _ * _ }.denominator
  }

  println(solution0)
}