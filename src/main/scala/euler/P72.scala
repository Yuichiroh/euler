package euler

/** Counting fractions
  * Problem 72
  * Consider the fraction, n/d, where n and d are positive integers. If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
  *
  * If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
  *
  * 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
  *
  * It can be seen that there are 21 elements in this set.
  *
  * How many elements would be contained in the set of reduced proper fractions for d ≤ 1,000,000?
  */
object P72 extends App {
  def solution0 = {
    val f = Factorization(limit)
    def phi(n: Int) = (f.factorize(n).map(f => 1 - 1.0 / f.p).product.toDouble * n).round.toLong
    (2 to limit).map(n => phi(n)).sum
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  val limit = args(1).toInt
  def solution = sId match {
    case 0 => solution0
  }
  println(solution)
}