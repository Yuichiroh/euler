package yuima.euler

import scala.collection.mutable
import java.math.MathContext

/** Square root digital expansion
  * Problem 80
  * It is well known that if the square root of a natural number is not an integer, then it is irrational.
  * The decimal expansion of such square roots is infinite without any repeating pattern at all.
  *
  * The square root of two is 1.41421356237309504880..., and the digital sum of the first one hundred decimal digits is 475.
  *
  * For the first one hundred natural numbers,
  * find the total of the digital sums of the first one hundred decimal digits for all the irrational square roots.
  */
object P80 extends App {
  implicit class RichBigDecimal(val r: BigDecimal) {
    def sqrt: BigDecimal = {
      val iteration = r.mc.getPrecision
      next(r, math.sqrt(r.toDouble), iteration)
    }

    private[this] def next(s: BigDecimal, guess: BigDecimal, remaining: Int): BigDecimal = {
      val updated = ((s / guess) + guess) / 2
      if (remaining < 0 || guess == updated) guess
      else next(s, updated, remaining - 1)
    }
  }

  private[this] def digitSum(n: BigDecimal, size: Int) =
    n.toString().split('.')(0).toCharArray().map(_.toString.toInt).take(size).sum

  val limit = if (args.size > 0) args(0).toInt else 100

  val squares = Stream.from(1).map(BigInt(_).pow(2))

  val irrationalSquareRoots =
    Iterator.from(0).flatMap(i => (squares(i) + 1 until squares(i + 1))).takeWhile(_ <= limit)
      .map(n => BigDecimal(n, new MathContext(100 + math.sqrt(limit).toInt)).sqrt)

  val result = irrationalSquareRoots.map(d => (d * math.pow(10, 99))).map(digitSum(_, 100)).sum
  println(result)
}