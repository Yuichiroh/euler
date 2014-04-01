package nlp.scala.euler

import nlp.scala.util.Stopwatch
import nlp.scala.util.Stopwatch

object P29 extends App {
  /**
   * Consider all integer combinations of a^b for 2 ≤ a ≤ 5 and 2 ≤ b ≤ 5:
   *
   * 2^2=4, 2^3=8, 2^4=16, 2^5=32
   * 3^2=9, 3^3=27, 3^4=81, 3^5=243
   * 4^2=16, 4^3=64, 4^4=256, 4^5=1024
   * 5^2=25, 5^3=125, 5^4=625, 5^5=3125
   * If they are then placed in numerical order, with any repeats removed, we get the following sequence of 15 distinct terms:
   *
   * 4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125
   *
   * How many distinct terms are in the sequence generated by a^b for 2 ≤ a ≤ 100 and 2 ≤ b ≤ 100?
   */

  val max = args(0).toInt

  /** greedy way */
  def solution1 = {
    val powers = for {
      a <- 2 to max
      b <- 2 to max
    } yield BigInt(a).pow(b)
    powers.distinct.size.toString
  }

  def perfectPowers(n: Int) = Stream.from(1).map(BigInt(n).pow(_))
  def perfectPowersList(max: Int) = Stream.from(2).map(perfectPowers(_).takeWhile(_ <= max))

  /** べき乗数のところだけ全列挙して重複チェック */
  def solution2 = {
    val ppBases = perfectPowersList(max).takeWhile(_.size > 1).flatten.distinct
    val ppPowers = for {
      a <- ppBases
      b <- 2 to max
    } yield a.pow(b)
    (max - 1 - ppBases.size) * (max - 1) + ppPowers.distinct.size
  }

  val sw = new Stopwatch
  args(1) match {
    case "1" => sw.time(solution1, "s1")
    case "2" => sw.time(solution2, "s2")
    case _ =>
    // TODO: べき乗数の重複の仕方の規則を実装する
  }

}