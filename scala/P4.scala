package euler.scala

import scala.collection.mutable.PriorityQueue

object P4 extends App {
  /**
   * Largest palindrome product
   * Problem 4
   * A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
   *
   * Find the largest palindrome made from the product of two 3-digit numbers.
   */

  def isPalindrome(num: String) = num.reverse == num

  def solution0 = (
    for {
      x <- 100 to 999
      y <- x to 999
      num = x * y
      if isPalindrome(num.toString)
    } yield num
  ).max

  /**
   * priority queueを使って積が大きくなる順に計算するほうが速い。
   * priority queueはデフォルトで降順。タプルの順序は先頭から値が比較される。
   */
  def solution1 = {
    val pq = PriorityQueue.newBuilder[(Int, Int, Int)]
    pq.enqueue((999 * 999, 999, 999))

    def products: Stream[Int] = {
      val (p, m, n) = pq.dequeue
      pq.enqueue((m * (n - 1), m, n - 1))
      if (m == n) pq.enqueue(((m - 1) * (n - 1), m - 1, n - 1))
      p #:: products
    }
    products.dropWhile(p => !isPalindrome(p.toString)).head
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}