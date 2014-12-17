package yuima.euler

object P36 extends App {
  /**
   * Double-base palindromes
   * Problem 36
   * The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.
   *
   * Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.
   *
   * (Please note that the palindromic number, in either base, may not include leading zeros.)
   */

  def isPalindrome(num: String) = num.reverse == num

  def solution0 = {
    val bound = 999 //1,000,000 -1 = 6桁 = 3桁 * 2
    val boundSize = bound.toString.size
    val palindromes = Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9) ++
      (1 to bound).flatMap(n => {
        val nStr = n.toString
        if (nStr.size < boundSize) (nStr + nStr.reverse).toInt +: (0 to 9).map(m => (nStr + m + nStr.reverse).toInt)
        else (nStr + nStr.reverse).toInt +: Nil
      })
    palindromes.filter(n => isPalindrome(n.toBinaryString)).sum
  }

  println(solution0)
}