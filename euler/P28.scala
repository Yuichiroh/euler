package euler.scala

import scala.collection.immutable.Stream.consWrapper

object P28 extends App {
  /**
   * Starting with the number 1 and moving to the right in a clockwise direction a 5 by 5 spiral is formed as follows:
   *
   * 21 22 23 24 25
   * 20  7  8  9 10
   * 19  6  1  2 11
   * 18  5  4  3 12
   * 17 16 15 14 13
   *
   * It can be verified that the sum of the numbers on the diagonals is 101.
   * What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed in the same way?
   */

  // １辺がkの正方形の角の部分にある数の和を求める: 1, 24, 76, ...
  // 角の値の最小値 a_k ＝ k-2の正方形の角の最大値 b_k-2 + (k-1)
  // 角同士は(k-1)の差: a_k, a_k + k-1, a_k + 2(k-1), a_k + 3(k-1)
  // 角の値の最大値 b_k = a_k + 3(k-1)
  // 角の和 s_k ＝ 4a_k + 6(k-1) = 4(b_k-2 + k -1) + 6(k -1) = 4b_k-2 + 10(k-1)
  // a_k = (s_k - 6(k-1)) / 4
  // b_k = (s_k + 6(k-1)) / 4
  // s_k = 4b_k-2 + 10(k-1) = s_k-2 + 6(k-3) + 10(k-1) = s_k-2 + 16k -28

  // sumは、一辺がkの正方形の角の合計値として定義するため、角が存在しない長さ１の時は成り立たなくなる。再帰計算のために仮想的に角があるとして、初期値を４と定義する。
  def sumOfSumOfCorner(lengthOfSide: Int, sum: Int, sumsum: Int): Stream[Int] = {
    val nextSum = sum + 16 * (lengthOfSide + 2) - 28
    sumsum #:: sumOfSumOfCorner(lengthOfSide + 2, nextSum, sumsum + nextSum)
  }

  println(sumOfSumOfCorner(1, 4, 1).drop(500).head)
}