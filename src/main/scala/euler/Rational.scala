package euler

import scala.runtime.ScalaRunTime
import scala.runtime.Statics

class Rational(val numerator: Int, val denominator: Int) extends Ordered[Rational] {
  require(denominator != 0)

  def this(num: Int) = this(num, 1)

  override def toString =
    if (denominator != 1) numerator + "/" + denominator
    else numerator.toString

  def *(f: Rational) = Rational(numerator * f.numerator, denominator * f.denominator)

  def +(f: Rational) = Rational(numerator * f.denominator + denominator * f.numerator, denominator * f.denominator)

  def ==(f: Rational) = if (numerator == f.numerator && denominator == f.denominator) true else false

  override def compare(that: Rational) = (this.numerator * that.denominator) compare (that.numerator * this.denominator)

  override def hashCode(): Int = {
    (denominator.hashCode() + 31) * 31 + numerator.hashCode()
  }

  override def equals(obj: Any) = {
    if (obj.isInstanceOf[Rational]) {
      val that = obj.asInstanceOf[Rational]
      if ((this.numerator * that.denominator) == (that.numerator * this.denominator)) true else false
    }
    else false
  }
}

object Rational {
  def apply(num: Int, den: Int) = {
    val d = gcd(num.abs, den.abs)
    new Rational(num / d, den / d)
  }

  def apply(num: Int) = new Rational(num, 1)

  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  implicit def intToRational(n: Int): Rational = Rational(n)
}