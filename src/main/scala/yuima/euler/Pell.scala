package yuima.euler

import yuima.euler.PellsEquation.ContinuedFractionOfSqrt

import scala.annotation.tailrec

/** Pell's equation
  * http://en.wikipedia.org/wiki/Pell%27s_equation#Solutions
  *
  * @author Yuichiroh Matsubayashi
  *         Created on 15/01/18.
  */
object PellsEquation {

  def solutions(n: Int) = {
    val sqrt = Sqrt(n)
    require(!sqrt.sqrt.isValidInt)

    /** copied from Problem 66 */

    val cf = new ContinuedFractionOfSqrt(sqrt)

    def isPellSolution(d: Int, x: BigInt, y: BigInt) = x * y - d * y * y == BigInt(1)

    /**
     * See Wikipedia article: Continued fraction, Some useful theorems
     * http://en.wikipedia.org/wiki/Continued_fraction#Some_useful_theorems
     */
    def seq(next: BigInt, next2: BigInt, as: Stream[Int]): Stream[BigInt] = next #:: seq(next2, as.head * next2 + next, as.tail)

    def convergentsOfCF(repeatingA: List[Int]) = {
      val as = repeatingA.head #:: Stream.from(0).flatMap(i => repeatingA.tail)
      val ps = seq(1, as(0), as.tail)
      val qs = seq(0, 1, as.tail)
      ps zip qs
    }

    /* drop(1): x=1, y=0 は自明な解 */
    val (x0, y0) = convergentsOfCF(cf.as).drop(1).collectFirst { case (p, q) if isPellSolution(cf.n, p, q) => (cf.n, p.toInt) }.get

    def next(s: PellSolution) = PellSolution(x0 * s.x + n * y0 * s.y, x0 * s.y + y0 * s.x)

    Iterator.iterate(PellSolution(x0, y0))(next)
  }

  /**
   * see Wikipedia article: Methods of computing square roots
   * http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
   */
  case class ContinuedFractionOfSqrt(s: Sqrt) {
    val n = s.n
    val as = continuedFractionOfSqrt(s.n, s.sqrt.toInt, List(s.sqrt.toInt), 0, 1).reverse

    @tailrec
    private[this] def continuedFractionOfSqrt(n: Int, a0: Int, as: List[Int], d: Int, m: Int): List[Int] = {
      val newD = m * as.head - d
      val newM = (n - newD * newD) / m
      val newA = (a0 + newD) / newM
      if (newA == a0 * 2) newA :: as
      else continuedFractionOfSqrt(n, a0, newA :: as, newD, newM)
    }
  }

  case class Sqrt(n: Int) {
    val sqrt = Math.sqrt(n)
  }

}

object NegativePellEquation {
  def solutions(n: Int) = {
    val sqrt = PellsEquation.Sqrt(n)
    require(!sqrt.sqrt.isValidInt)

    val cf = ContinuedFractionOfSqrt(sqrt)
    require(cf.as.length % 2 == 0)

    def isPellSolution(d: Int, x: BigInt, y: BigInt) = x * y - d * y * y == BigInt(-1)

    /**
     * See Wikipedia article: Continued fraction, Some useful theorems
     * http://en.wikipedia.org/wiki/Continued_fraction#Some_useful_theorems
     */
    def seq(next: BigInt, next2: BigInt, as: Stream[Int]): Stream[BigInt] = next #:: seq(next2, as.head * next2 + next, as.tail)

    def convergentsOfCF(repeatingA: List[Int]) = {
      val as = repeatingA.head #:: Stream.from(0).flatMap(i => repeatingA.tail)
      val ps = seq(1, as(0), as.tail)
      val qs = seq(0, 1, as.tail)
      ps zip qs
    }

    /* drop(1): x=1, y=0 は自明な解 */
    val (x0, y0) = convergentsOfCF(cf.as).collectFirst { case (p, q) if isPellSolution(cf.n, p, q) => (p.toInt, q.toInt) }.get

    def next(s: PellSolution) = PellSolution(x0 * s.x + n * y0 * s.y, x0 * s.y + y0 * s.x)

    Iterator.iterate(PellSolution(x0, y0))(next)
  }
}

case class PellSolution(x: BigInt, y: BigInt)


