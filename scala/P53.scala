package euler.scala

object P53 extends App {
  /**
   * Combinatoric selections
   * Problem 53
   * There are exactly ten ways of selecting three from five, 12345:
   *
   * 123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
   *
   * In combinatorics, we use the notation, 5C3 = 10.
   *
   * In general,
   *
   * nCr =
   * n!
   * r!(n−r)!
   * ,where r ≤ n, n! = n×(n−1)×...×3×2×1, and 0! = 1.
   * It is not until n = 23, that a value exceeds one-million: 23C10 = 1144066.
   *
   * How many, not necessarily distinct, values of  nCr, for 1 ≤ n ≤ 100, are greater than one-million?
   */

  val max = 1000000
  val nBound = 100

  /** 対象なので、rが小さい側だけ求める */
  def c(n: Int, r: Int = 0, prev: Int = 1): Stream[(Int, Int)] =
    if ((r + 1) <= n - r - 1) (prev, r) #:: c(n, r + 1, prev * (n - r) / (r + 1))
    else (prev, r) #:: Stream.empty[(Int, Int)]

  /** nCrの中で、与えられたrで決まる値以上となる要素の数 */
  def countGreaterVal(n: Int, r: Int) = n + 1 - 2 * r

  def findFirst(n: Int, r: Int = 0, prev: Int = 1): Option[Int] = {
    if ((r + 1) <= n - r - 1) {
      if (prev > max) Some(r)
      else findFirst(n, r + 1, prev * (n - r) / (r + 1))
    }
    else None
  }

  def solution0 = (1 to 100).map(n => (n, findFirst(n))).withFilter(_._2 != None).map(t => countGreaterVal(t._1, t._2.get)).sum

  println(solution0)
}