package euler

object P40 extends App {
  /**
   * Champernowne's constant
   * Problem 40
   * An irrational decimal fraction is created by concatenating the positive integers:
   *
   * 0.123456789101112131415161718192021...
   *
   * It can be seen that the 12th digit of the fractional part is 1.
   *
   * If dn represents the nth digit of the fractional part, find the value of the following expression.
   *
   * d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
   */

  def int2seq(n: Int) = n.toString.map(_.toString.toInt)

  def numStream = Iterator.from(1).flatMap(int2seq)

  def solution0 = numStream.zipWithIndex.takeWhile(_._2 < 1000000)
    .collect{case n if (n._2 + 1) % 10 == 0 && Math.log10(n._2 + 1).isValidInt => n._1}.product

  println(solution0)
}