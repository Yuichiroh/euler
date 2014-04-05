package euler.scala

import scala.annotation.tailrec

object P38 extends App {
  /**
   * Pandigital multiples
   * Problem 38
   * Take the number 192 and multiply it by each of 1, 2, and 3:
   *
   * 192 × 1 = 192
   * 192 × 2 = 384
   * 192 × 3 = 576
   *
   * By concatenating each product we get the 1 to 9 pandigital, 192384576.
   * We will call 192384576 the concatenated product of 192 and (1,2,3)
   *
   * The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and 5, giving the pandigital, 918273645,
   * which is the concatenated product of 9 and (1,2,3,4,5).
   *
   * What is the largest 1 to 9 pandigital 9-digit number that can be formed as the concatenated product of an integer with (1,2, ... , n)
   * where n > 1?
   */

  def concatenatedProduct(n: Int, m: Int) = multiples(n).take(m).flatMap(_.toString.toCharArray.map(_.toString.toInt))

  def multiples(n: Int) = Stream.from(1).map(n * _)

  @tailrec
  def isPandigital(n: Int, m: Int = 1): (Boolean, Int, Int, Int) = {
    val concat = concatenatedProduct(n, m)
    val size = concat.size
    if (size > 9) (false, 0, n, m)
    else if (size < 9) isPandigital(n, m + 1)
    else if (concat.toList.sorted == List(1, 2, 3, 4, 5, 6, 7, 8, 9)) (true, concat.mkString("").toInt, n, m)
    else (false, 0, n, m)
  }

  /**
   * n = 2のとき、倍数の元の数が最大の自然数を取る。
   * このとき、各数字を１回のみ含むパンデジタルである必要条件はこの最初の数が四桁であること。
   * (4桁 * 1 = 4桁, 4桁 * 2 = 5桁)で9桁を実現しなければならない。
   */
  def solution0 = (1 to 9999).map(isPandigital(_)).filter(_._1).map(_._2).max

  println(solution0)
}