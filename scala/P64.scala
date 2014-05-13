package euler.scala

import scala.annotation.tailrec

object P64 extends App {
  /**
   * See https://projecteuler.net/problem=64
   *
   * All square roots are periodic when written as continued fractions and can be written in the form:
   *
   * √N = a0 + 1 / (a1 +  1 / (a2 +  1 / (a3 +  ...
   *
   * For example, let us consider √23:
   *
   * √23 = 4 + √23 — 4 = 4 + (1 / 1 / √23—4) = 4 + 1 / ( 1 + (√23 – 3) / 7 )
   *
   * If we continue we would get the following expansion:
   *
   * (omitted)
   *
   * The process can be summarised as follows:
   *
   * (omitted)
   *
   * It can be seen that the sequence is repeating. For conciseness, we use the notation √23 = [4;(1,3,1,8)],
   * to indicate that the block (1,3,1,8) repeats indefinitely.
   *
   * The first ten continued fraction representations of (irrational) square roots are:
   *
   * √2=[1;(2)], period=1
   * √3=[1;(1,2)], period=2
   * √5=[2;(4)], period=1
   * √6=[2;(2,4)], period=2
   * √7=[2;(1,1,1,4)], period=4
   * √8=[2;(1,4)], period=2
   * √10=[3;(6)], period=1
   * √11=[3;(3,6)], period=2
   * √12= [3;(2,6)], period=2
   * √13=[3;(1,1,1,1,6)], period=5
   *
   * Exactly four continued fractions, for N ≤ 13, have an odd period.
   *
   * How many continued fractions for N ≤ 10000 have an odd period?
   */

  /**
   * see Wikipedia article: Methods of computing square roots
   * http://en.wikipedia.org/wiki/Methods_of_computing_square_roots#Continued_fraction_expansion
   */
  @tailrec
  def continuedFractionOfRoot(n: Int, a0: Int, as: List[Int], d: Int, m: Int): List[Int] = {
    val newD = m * as.head - d
    val newM = (n - newD * newD) / m
    val newA = ((a0 + newD) / newM).toInt
    if (newA == a0 * 2) as
    else continuedFractionOfRoot(n, a0, newA :: as, newD, newM)
  }

  val max = if (args.size > 0) args(0).toInt else 10000

  def solution0 = (2 to max).filter(n => {
    val a = Math.sqrt(n)
    !a.isValidInt && continuedFractionOfRoot(n, a.toInt, List(a.toInt), 0, 1).length % 2 == 1
  }).size

  println(solution0)
}