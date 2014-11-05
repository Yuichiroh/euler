package euler

object P68 extends App {
  /** Magic 5-gon ring
    * Problem 68
    * Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.
    *
    *
    * Working clockwise, and starting from the group of three with the numerically lowest external node (4,3,2 in this example),
    * each solution can be described uniquely. For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.
    *
    * It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.
    *
    * Total	Solution Set
    * 9	4,2,3; 5,3,1; 6,1,2
    * 9	4,3,2; 6,2,1; 5,1,3
    * 10	2,3,5; 4,5,1; 6,1,3
    * 10	2,5,3; 6,3,1; 4,1,5
    * 11	1,4,6; 3,6,2; 5,2,4
    * 11	1,6,4; 5,4,2; 3,2,6
    * 12	1,5,6; 2,6,4; 3,4,5
    * 12	1,6,5; 3,5,4; 2,4,6
    * By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.
    *
    * Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings.
    * What is the maximum 16-digit string for a "magic" 5-gon ring?
    */

  /** nodes in n-gon ring is 2n.
    * sum of outer nodes is multiple of n.
    * sum of inner ring nodes is multiple of n.
    * sum_o + sum_i = n * (2n + 1)
    * n * sum_l = sum_o + 2 * sum_i <=> sum_i = n * (sum_l - 2n - 1)
    */

  def outers(n: Int) = (1 to 2 * n).combinations(n).withFilter(_.sum % n == 0)
    .flatMap(_.permutations).withFilter(o => o(0) == o.min)

  def inners(n: Int, outer: Seq[Int]) = (1 to 2 * n).diff(outer).permutations

  case class Ring(n: Int, outer: Seq[Int], inner: Seq[Int])

  def validRing(r: Ring) = {
    val validLineSum = 2 * r.n + 1 + r.inner.sum / r.n
    (0 until r.n).forall(i => r.outer(i) + r.inner(i) + r.inner((i + 1) % r.n) == validLineSum)
  }

  def nGonRing(r: Ring) = {
    val ngon = new StringBuilder
    (0 until r.n).foreach { i =>
      ngon append r.outer(i)
      ngon append r.inner(i)
      ngon append r.inner((i + 1) % r.n)
    }
    ngon.toString
  }

  def nGonRings(n: Int) = outers(n).flatMap(o => inners(n, o).map(i => Ring(n, o, i)))
    .filter(validRing).map(nGonRing)

  def solution = nGonRings(5).filter(_.size == 16).maxBy(_.toLong)

  println(solution)
}