package nlp.scala.euler

object P26 extends App {
  /**
   * A unit fraction contains 1 in the numerator.
   * The decimal representation of the unit fractions with denominators 2 to 10 are given:
   *
   * 1/2	= 	0.5
   * 1/3	= 	0.(3)
   * 1/4	= 	0.25
   * 1/5	= 	0.2
   * 1/6	= 	0.1(6)
   * 1/7	= 	0.(142857)
   * 1/8	= 	0.125
   * 1/9	= 	0.(1)
   * 1/10	= 	0.1
   * Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.
   *
   * Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
   */
  // 有理数が有限小数表示を持つのは、分母の素因数が 2, 5（一般には、基数の約数たる素数）のみであるときで、またそのときに限る。現在未使用。

  def recurringCycleOfUnitFraction(denominator: Int, quotients: List[Int] = List(0), remainings: List[Int] = List(1)): List[Int] = {
    val remaining = (remainings.head % denominator) * 10
    if (remaining == 0) Nil
    else {
      if (remainings.contains(remaining)) quotients.take(remainings.indexOf(remaining) + 1)
      else recurringCycleOfUnitFraction(denominator, remainings.head / denominator :: quotients, remaining :: remainings)
    }
  }
  val lengths = (1 until 1000).map(recurringCycleOfUnitFraction(_).length)
  // 0番目は1/1を表す
  println(lengths.indexOf(lengths.max) + 1)
}