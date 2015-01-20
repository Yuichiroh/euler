package yuima.euler

/**
 * @author Yuichiroh Matsubayashi
 *         Created on 14/12/19.
 */
object PrimitivePythagoreanTripleTree {
  def children(a: Int, b: Int, c: Int) = Seq(
    Seq(2 * a + b - c, -2 * a + 2 * b + 2 * c, -2 * a + b + 3 * c),
    Seq(2 * a + b + c, 2 * a - 2 * b + 2 * c, 2 * a - b + 3 * c),
    Seq(2 * a - b + c, 2 * a + 2 * b + 2 * c, 2 * a + b + 3 * c)
  )
}

object PythagoreanTriple {
  def pythagoreanTriple(m: Int, n: Int) = {
    require(n <= m)
    val m2 = math.pow(m, 2).toInt
    val n2 = math.pow(n, 2).toInt
    (m2 - n2, 2 * m * n, m2 + n2)
  }
}