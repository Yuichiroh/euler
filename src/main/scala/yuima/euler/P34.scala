package yuima.euler

import scala.Array.canBuildFrom
import scala.language.postfixOps

object P34 extends App {
  /**
   * Digit factorials
   * Problem 34
   * 145 is a curious number, as 1! + 4! + 5! = 1 + 24 + 120 = 145.
   *
   * Find the sum of all numbers which are equal to the sum of the factorial of their digits.
   *
   * Note: as 1! = 1 and 2! = 2 are not sums they are not included.
   */

  implicit class RichInt(val n: Int) {
    def ! : Int = n match {
      case -1 => 0
      case 0 => 1
      case _ => (1 to n).product
    }
  }

  def solution0 = {
    val max = Iterator.from(1).dropWhile(n => ((9!) * n).toString.size >= n).next
    val factorials = (0 to 9).map(_!)
    Iterator.from(10).takeWhile(_.toString.size < max).
      filter(n =>
        n.toString.map(n => factorials(n.toString.toInt)).sum == n
      ).sum
  }

  def removePrefix(num: List[Int]): List[Int] = num match {
    case -1 :: rest => removePrefix(rest)
    case _ => num
  }

  /** 乗数の和が同じになる数の計算を省く */
  def solution1 = {
    val max = Iterator.from(1).dropWhile(n => ((9!) * n).toString.size >= n).next - 1
    val factorials = (-1 to 9).map(_!)
    val digitsCombinations = (-1 to 9).flatMap(Seq.fill(max)(_)).combinations(max).map(e => removePrefix(e.toList))
    val sums = for {
      combination <- digitsCombinations
      sum = combination.map(n => factorials(n + 1)).sum
      newList = sum.toString.map(_.toString.toInt).toList.sorted
      if combination.equals(newList)
    } yield sum
    sums.sum - 3 // 1! と 2!を除外
  }

  def solution2 = {
    val max = Iterator.from(1).dropWhile(n => ((9!) * n).toString.size >= n).next - 1
    val factorials = (0 to 9).map(_!)
    val digitsCombinations = (2 to max).flatMap(n => (0 to 9).flatMap(List.fill(n)(_)).combinations(n))
    val sums = for {
      combination <- digitsCombinations
      sum = combination.map(n => factorials(n)).sum
      newList = sum.toString.map(_.toString.toInt).toList.sorted
      if combination.equals(newList)
    } yield sum
    sums.sum
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
    case 2 => solution2
  }
  println(solution)
}