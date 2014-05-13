package euler.scala

import scala.annotation.tailrec

object P66 extends App {
  /**
   * Diophantine equation
   * Problem 66
   * Consider quadratic Diophantine equations of the form:
   *
   * x2 – Dy2 = 1
   *
   * For example, when D=13, the minimal solution in x is 6492 – 13×1802 = 1.
   *
   * It can be assumed that there are no solutions in positive integers when D is square.
   *
   * By finding minimal solutions in x for D = {2, 3, 5, 6, 7}, we obtain the following:
   *
   * 32 – 2×22 = 1
   * 22 – 3×12 = 1
   * 92 – 5×42 = 1
   * 52 – 6×22 = 1
   * 82 – 7×32 = 1
   *
   * Hence, by considering minimal solutions in x for D ≤ 7, the largest x is obtained when D=5.
   *
   * Find the value of D ≤ 1000 in minimal solutions of x for which the largest value of x is obtained.
   */

  val max = if (args.size > 0) args(0).toInt else 1000

  def isPellSolution(d: Int, x: BigInt, y: BigInt) = (x * y - d * y * y) == 1

  /**
   * Pell's equation:
   * http://en.wikipedia.org/wiki/Pell%27s_equation#Solutions
   */
  def solution0 = continuedFractionsOfSqrts.map(cf =>
    /* drop(1): x=1, y=0 は自明な解 */
    convergentsOfCF(cf.as).drop(1).collectFirst { case (p, q) if isPellSolution(cf.n, p, q) => (cf.n, p) }.get
  ).maxBy(dx => dx._2)._1

  println(solution0)

  /**
   * see Wikipedia article: Methods of computing square roots
   * http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
   */
  class ContinuedFraqtionOfSqrt(s: Sqrt) {
    val n = s.n
    val as = continuedFractionOfSqrt(s.n, s.sqrt.toInt, List(s.sqrt.toInt), 0, 1).reverse

    @tailrec
    private def continuedFractionOfSqrt(n: Int, a0: Int, as: List[Int], d: Int, m: Int): List[Int] = {
      val newD = m * as.head - d
      val newM = (n - newD * newD) / m
      val newA = ((a0 + newD) / newM).toInt
      if (newA == a0 * 2) newA :: as
      else continuedFractionOfSqrt(n, a0, newA :: as, newD, newM)
    }
  }

  case class Sqrt(n: Int) { val sqrt = Math.sqrt(n) }

  def continuedFractionsOfSqrts = (2 to max).map(Sqrt).withFilter(!_.sqrt.isValidInt)
    .map(new ContinuedFraqtionOfSqrt(_))

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
}