package yuima.euler

/**
 * Counting rectangles
 * Problem 85
 * By counting carefully it can be seen that a rectangular grid measuring 3 by 2 contains eighteen rectangles:
 *
 * Although there exists no rectangular grid that contains exactly two million rectangles,
 * find the area of the grid with the nearest solution.
 */
object P85 extends App {

  val target = 2000000
  println(solution)

  /** counting rectangles inside an m * n rectangle is reduced to counting possible selection of
    * two sides of small rectangle from all the sides of grids. */
  def rectangles(m: Int)(n: Int) = (m + 1) * m * (n + 1) * n / 4

  def search(m: Int, n: Int, minM: Int, minN: Int, minDiff: Int): Seq[Int] = {
    if (minDiff == 0) Seq(minM, minN)
    else if (m >= n) rectangles(m)(n) - target match {
      case diff if diff > 0 =>
        if (diff < minDiff) search(m - 1, n, m, n, diff)
        else search(m - 1, n, minM, minN, minDiff)
      case diff if diff < 0 =>
        if (-diff < minDiff) search(m, n + 1, m, n, -diff)
        else search(m, n + 1, minM, minN, minDiff)
      case zero => Seq(m, n)
    }
    else Seq(minM, minN)
  }

  def solution = search(2000, 1, 2000, 1, 2000000).product
}
