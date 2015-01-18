package yuima.euler

/** Arranged probability
  * Problem 100
  *
  * If a box contains twenty-one coloured discs, composed of fifteen blue discs and six red discs,
  * and two discs were taken at random, it can be seen that the probability of taking two blue discs, P(BB) = (15/21)×(14/20) = 1/2.
  *
  * The next such arrangement, for which there is exactly 50% chance of taking two blue discs at random,
  * is a box containing eighty-five blue discs and thirty-five red discs.
  *
  * By finding the first arrangement to contain over 10^12 = 1,000,000,000,000 discs in total,
  * determine the number of blue discs that the box would contain.
  */
object P100 extends App {
  /** (x/y) * ((x-1)/(y-1)) = 1/2
    * -2 * (2 * x - 1)^2 + (2 * y - 1)^2 = -1
    * */
  def solution = {
    /** x is the number of all discs and y is the number of blue discs. */
    def pell2solution(s: PellSolution) = {
      val x = BigDecimal(s.x + 1) / 2.0
      val y = BigDecimal(s.y + 1) / 2.0
      if (x.isValidLong && y.isValidLong) Some((x.toLong, y.toLong))
      else None
    }

    val ss = NegativePellEquation.solutions(2).map(pell2solution).collect { case Some(s) => s }
    ss.dropWhile(_._1 < 1000000000000L).next()._2
  }

  println(solution)
}
