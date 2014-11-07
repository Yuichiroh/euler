package euler

import scala.annotation.tailrec

/** Digit factorial chains
  * Problem 74
  * The number 145 is well known for the property that the sum of the factorial of its digits is equal to 145:
  *
  * 1! + 4! + 5! = 1 + 24 + 120 = 145
  *
  * Perhaps less well known is 169, in that it produces the longest chain of numbers that link back to 169; it turns out that there are only three such loops that exist:
  *
  * 169 → 363601 → 1454 → 169
  * 871 → 45361 → 871
  * 872 → 45362 → 872
  *
  * It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,
  *
  * 69 → 363600 → 1454 → 169 → 363601 (→ 1454)
  * 78 → 45360 → 871 → 45361 (→ 871)
  * 540 → 145 (→ 145)
  *
  * Starting with 69 produces a chain of five non-repeating terms, but the longest non-repeating chain with a starting number below one million is sixty terms.
  *
  * How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?
  */
object P74 extends App {
  implicit class RichInt(val n: Int) {
    def ! : Int = (1 to n).product
  }

  private[this] def factorialSum(n: Int) = digits(n).map(_!).sum

  private[this] def digits(n: Int): List[Int] = n match {
    case 0 => Nil
    case _ => (n % 10) :: digits(n / 10)
  }

  //  factorialSum(999999) => 2177280
  //  factorialSum(2177280) => 50406
  private[this] val repeating = new Array[Int](2177281)

  @tailrec
  def length(n: Int, len: Int = 0, chain: List[Int] = Nil): Int = {
    if (repeating(n) > 0) {
      chain.foreach { num => repeating(num) = repeating(n) + len + repeating(num) + 1 }
      len + repeating(n)
    }
    else if (repeating(n) < 0) {
      val loopLen = repeating(n)
      chain.foreach { num =>
        repeating(num) =
          if (repeating(num) > loopLen) len + repeating(num) + 1
          else len + loopLen + 1
      }
      len
    }
    else {
      repeating(n) = -len - 1
      length(factorialSum(n), len + 1, n :: chain)
    }
  }

  val solution = (1 until 1000000).count(n => length(n) == 60)
  println(solution)
}