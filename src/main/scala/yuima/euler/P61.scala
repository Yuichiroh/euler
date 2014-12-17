package yuima.euler

import scala.annotation.tailrec

object P61 extends App {
  /**
   * Cyclical figurate numbers
   * Problem 61
   * Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers are all figurate (polygonal) numbers
   * and are generated by the following formulae:
   *
   * Triangle	 	P3,n=n(n+1)/2	 	1, 3, 6, 10, 15, ...
   * Square	 	P4,n=n^2	 	1, 4, 9, 16, 25, ...
   * Pentagonal	 	P5,n=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
   * Hexagonal	 	P6,n=n(2n−1)	 	1, 6, 15, 28, 45, ...
   * Heptagonal	 	P7,n=n(5n−3)/2	 	1, 7, 18, 34, 55, ...
   * Octagonal	 	P8,n=n(3n−2)	 	1, 8, 21, 40, 65, ...
   * The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three interesting properties.
   *
   * The set is cyclic, in that the last two digits of each number is the first two digits of the next number
   * (including the last number with the first).
   * Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and pentagonal (P5,44=2882),
   * is represented by a different number in the set.
   * This is the only set of 4-digit numbers with this property.
   * Find the sum of the only ordered set of six cyclic 4-digit numbers for which each polygonal type:
   * triangle, square, pentagonal, hexagonal, heptagonal, and octagonal, is represented by a different number in the set.
   */

  def polygonals(k: Int) = Stream.from(1).map(n => (n * ((k - 2) * n + 4 - k) / 2, k)).dropWhile(_._1 < 1011).takeWhile(_._1 < 10000)

  val numbers = (3 to 7).flatMap(polygonals)

  @tailrec
  def find(depth: Int, lists: Seq[Seq[(Int, Int)]]): Seq[Seq[(Int, Int)]] = depth match {
    case 1 => lists
    case _ => find(depth - 1,
      lists.flatMap(list =>
        numbers.filter(n => list.forall(poly => poly._2 != n._2 && poly._1 != n._1) && n._1 / 100 == list.head._1 % 100)
          .map(_ +: list)
      )
    )
  }

  def solution0 = find(6, polygonals(8).map(Seq(_)))
    .filter(list => list.head._1 % 100 == list.last._1 / 100).head
    .map(_._1).sum

  println(solution0)
}