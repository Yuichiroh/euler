package yuima.euler

/** Counting fractions in a range
  * Problem 73
  * Consider the fraction, n/d, where n and d are positive integers.
  * If n<d and HCF(n,d)=1, it is called a reduced proper fraction.
  *
  * If we list the set of reduced proper fractions for d ≤ 8 in ascending order of size, we get:
  *
  * 1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8
  *
  * It can be seen that there are 3 fractions between 1/3 and 1/2.
  *
  * How many fractions lie between 1/3 and 1/2 in the sorted set of reduced proper fractions for d ≤ 12,000?
  */
object P73 extends App {

  def solution0 = {
    (5 to limit).map(d => (d / 3 + 1 until (d + 1) / 2).count(n => gcd(n, d) == 1)).sum
  }

  /** Culculation of "next term" in Farey sequence.
    * See Wikipedia: http://www.wikiwand.com/en/Farey_sequence
    */
  def solution1 = {
    def nextTermOfFareySequence(f1: (Int, Int), f2: (Int, Int), n: Int): (Int, Int) = {
      val k = (n + f1._2) / f2._2
      val num = k * f2._1 - f1._1
      val den = k * f2._2 - f1._2
      (num, den)
    }

    def find(f1: (Int, Int), f2: (Int, Int), n: Int, count: Int = 0): Int = {
      if (f2._2.toDouble / f2._1 == 2.0) count
      else find(f2, nextTermOfFareySequence(f1, f2, n), n, count + 1)
    }

    val nextToOneThird = (5 to limit).map(d => (d / 3 + 1, d)).minBy(f => f._1.toDouble / f._2)
    find((1, 3), nextToOneThird, limit)
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  val limit = if (args.size > 1) args(1).toInt else 12000

  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)

}