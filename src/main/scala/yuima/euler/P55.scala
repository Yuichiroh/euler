package yuima.euler

object P55 extends App {
  /**
   * Lychrel numbers
   * Problem 55
   * If we take 47, reverse and add, 47 + 74 = 121, which is palindromic.
   *
   * Not all numbers produce palindromes so quickly. For example,
   *
   * 349 + 943 = 1292,
   * 1292 + 2921 = 4213
   * 4213 + 3124 = 7337
   *
   * That is, 349 took three iterations to arrive at a palindrome.
   *
   * Although no one has proved it yet, it is thought that some numbers, like 196, never produce a palindrome.
   * A number that never forms a palindrome through the reverse and add process is called a Lychrel number.
   * Due to the theoretical nature of these numbers, and for the purpose of this problem,
   * we shall assume that a number is Lychrel until proven otherwise.
   * In addition you are given that for every number below ten-thousand,
   * it will either (i) become a palindrome in less than fifty iterations, or, (ii) no one,
   * with all the computing power that exists, has managed so far to map it to a palindrome.
   * In fact, 10677 is the first number to be shown to require over fifty iterations before producing a palindrome:
   * 4668731596684224866951378664 (53 iterations, 28-digits).
   *
   * Surprisingly, there are palindromic numbers that are themselves Lychrel numbers; the first example is 4994.
   *
   * How many Lychrel numbers are there below ten-thousand?
   *
   * NOTE: Wording was modified slightly on 24 April 2007 to emphasise the theoretical nature of Lychrel numbers.
   */

  val max = if (args.length > 1) args(1).toInt else 10000
  val limit = 50

  def isPalindrome(num: String) = num.reverse == num

  def seq(n: BigInt): Stream[BigInt] = n #:: seq(n + BigInt(n.toString.reverse))

  def isLychrel(n: Int) = seq(n).drop(1).take(limit).forall(m => !isPalindrome(m.toString))

  val checked3 = collection.mutable.Map[Int, Boolean]()
  def isLychrel2(n: Int) = checked3.getOrElse(n, {
    val stream = seq(n).drop(1).take(limit).takeWhile(m => !isPalindrome(m.toString))
    if (stream.size < limit) {
      stream.takeWhile(_ < max).foreach(m => { checked3(m.toInt) = false; checked3(m.toString.reverse.toInt) = false })
      false
    }
    else {
      checked3(n.toString.reverse.toInt) = true
      true
    }
  })

  /** 全探索 */
  def solution0 = (1 to max).filter(isLychrel(_))

  def solution1 = (1 to max).filter(isLychrel2(_))

  val sId = if (args.size > 0) args(0).toInt else 0
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  val result = solution
  println(result.size)
}