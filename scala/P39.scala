package euler.scala

object P39 extends App {
  /**
   * Integer right triangles
   * Problem 39
   * If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.
   *
   * {20,48,52}, {24,45,51}, {30,40,50}
   *
   * For which value of p ≤ 1000, is the number of solutions maximised?
   */

  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  def coprime(m: Int, n: Int) = gcd(m, n) == 1

  def primitiveTriples = Stream.from(2).flatMap(m => Stream.from(1).takeWhile(_ < m).map(n => (m, n)).filter(p => coprime(p._1, p._2))).
    map(p => {
      val m2 = Math.pow(p._1, 2).toInt
      val n2 = Math.pow(p._2, 2).toInt
      val a = m2 - n2
      val b = 2 * p._1 * p._2
      val c = m2 + n2
      (a, b, c, a + b + c)
    })

  def tripletMultiples(t: (Int, Int, Int, Int)) = Stream.from(1).map(k => (k * t._1, k * t._2, k * t._3, k * t._4))

  def solution0 = primitiveTriples.takeWhile(_._4 <= 1000).flatMap(tripletMultiples(_).takeWhile(_._4 <= 1000)).groupBy(_._4).map(e => (e._1, e._2.size)).toList.maxBy(_._2)

  def solution1 = (Map[Int, Int]().withDefaultValue(0) /: primitiveTriples.takeWhile(_._4 <= 1000).flatMap(tripletMultiples(_).takeWhile(_._4 <= 1000))) { (count, t) => count + (t._4 -> (count(t._4) + 1)) }.maxBy(_._2)

  val sId = if (args.size > 0) args(0).toInt else 1
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}