package euler

object P57 extends App {
  /**
   * Square root convergents
   * Problem 57
   * It is possible to show that the square root of two can be expressed as an infinite continued fraction.
   *
   * √ 2 = 1 + 1/(2 + 1/(2 + 1/(2 + ... ))) = 1.414213...
   *
   * By expanding this for the first four iterations, we get:
   *
   * 1 + 1/2 = 3/2 = 1.5
   * 1 + 1/(2 + 1/2) = 7/5 = 1.4
   * 1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666...
   * 1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379...
   *
   * The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985,
   * is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.
   *
   * In the first one-thousand expansions, how many fractions contain a numerator with more digits than denominator?
   */

  class Fraction private (val numerator: BigInt, val denominator: BigInt) {
    override def toString = {
      if (denominator != 1) new StringBuilder(numerator.toString).append("/").append(denominator.toString).toString
      else numerator.toString
    }

    def *(f: Fraction) = Fraction(numerator * f.numerator, denominator * f.denominator)
    def +(f: Fraction) = Fraction(numerator * f.denominator + denominator * f.numerator, denominator * f.denominator)
    def ==(f: Fraction) = { if (numerator == f.numerator && denominator == f.denominator) true else false }
    def reverse = Fraction(denominator, numerator)

    implicit def FractionInt(n: BigInt): Fraction = new Fraction(n, 1)
  }

  object Fraction {
    def apply(num: BigInt, den: BigInt) = {
      val d = gcd(num, den)
      new Fraction(num / d, den / d)
    }

    def gcd(m: BigInt, n: BigInt): BigInt = if (n == 0) m else gcd(n, m % n)
  }

  val max = 1000

  def gcfExpantion4SquareRoot(n: Int, iter: Int): List[Fraction] = iter match {
    case 1 => List(Fraction(1, 2))
    case _ =>
      val prevList = gcfExpantion4SquareRoot(n, iter - 1)
      (Fraction(2, 1) + prevList.head).reverse :: prevList
  }

  def gcfExpantion4SquareRoot2(n: Int, iter: Int): (List[(BigInt, BigInt)]) = iter match {
    case 1 => List((3, 2))
    case _ =>
      val prevList = gcfExpantion4SquareRoot2(n, iter - 1)
      val denominator = prevList.head._1 + prevList.head._2
      val numerator = prevList.head._2 + denominator
      (numerator, denominator) :: prevList
  }

  def solution0 = gcfExpantion4SquareRoot(2, max).filter(f => f.denominator.toString.size < (f.numerator + f.denominator).toString.size).size

  def solution1 = gcfExpantion4SquareRoot2(2, max).filter(f => f._1.toString.size > f._2.toString.size).size

  val sId = if (args.size > 0) args(0).toInt else 1
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}