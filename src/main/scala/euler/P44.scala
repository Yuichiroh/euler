package euler

import scala.annotation.tailrec

object P44 extends App {
  /**
   * Pentagon numbers
   * Problem 44
   * Pentagonal numbers are generated by the formula, Pn=n(3n−1)/2. The first ten pentagonal numbers are:
   *
   * 1, 5, 12, 22, 35, 51, 70, 92, 117, 145, ...
   *
   * It can be seen that P4 + P7 = 22 + 70 = 92 = P8. However, their difference, 70 − 22 = 48, is not pentagonal.
   *
   * Find the pair of pentagonal numbers, Pj and Pk, for which their sum and difference are pentagonal and D = |Pk − Pj| is minimised;
   * what is the value of D?
   */

  /**
   * n以下の Pn を全てリストに保持しておく。Pn+1を生成して、リスト中の大きい方から和・差が五角数となっているのものを見つける。
   * 見つからなければn+2で探す
   * 見つかったら、その数をDとして、Pn+2 - Pn+1と比較、Dの方が小さければ探索終了
   * Dの方が大きければ、リスト中の大きい方から、差がDを超えない範囲で、和・差が五角数となっているのものを見つける。
   * 見つからなければ探索終了、見つかればDを上書きしてPn+3で同様の事を繰り返す。
   */

  val pentagonals = Stream.from(1).map(n => n * (3 * n - 1) / 2)

  def isPentagonal(n: Int) = Math.sqrt(24 * n + 1) % 6 == 5

  @tailrec
  def findFirst(n: Int = 1): (Int, Int) = {
    val p = pentagonals(n)
    pentagonals.slice(0, n).reverse.find(p2 => isPentagonal(p + p2) && isPentagonal(p - p2)) match {
      case Some(p2) => (n, p - p2)
      case None => findFirst(n + 1)
    }
  }

  @tailrec
  def min(n: Int, d: Int): Int = {
    val p = pentagonals(n)
    pentagonals.slice(0, n).reverse.takeWhile(p2 => p - p2 < d).find(p2 => isPentagonal(p + p2) && isPentagonal(p - p2)) match {
      case Some(p2) => min(n + 1, p - p2)
      case None => d
    }
  }

  def solution0 = {
    val (n, d) = findFirst()
    min(n + 1, d)
  }

  println(solution0)
}