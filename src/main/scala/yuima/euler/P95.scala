package yuima.euler

/** Amicable chains
  * Problem 95
  * The proper divisors of a number are all the divisors excluding the number itself.
  * For example, the proper divisors of 28 are 1, 2, 4, 7, and 14.
  * As the sum of these divisors is equal to 28, we call it a perfect number.
  *
  * Interestingly the sum of the proper divisors of 220 is 284 and the sum of the proper divisors of 284 is 220,
  * forming a chain of two numbers. For this reason, 220 and 284 are called an amicable pair.
  *
  * Perhaps less well known are longer chains. For example, starting with 12496, we form a chain of five numbers:
  *
  * 12496 → 14288 → 15472 → 14536 → 14264 (→ 12496 → ...)
  *
  * Since this chain returns to its starting point, it is called an amicable chain.
  * Find the smallest member of the longest amicable chain with no element exceeding one million.
  */
object P95 extends App {
  val max = if (args.length > 0) args(0).toInt else 1000000

  val lengths = Array.fill(max)(0)
  lengths(0) = -1
  lengths(1) = 1

  primeNumbers(max).foreach(p=> lengths(p) = -1)

  import scala.annotation.tailrec

  @tailrec
  def sociableNums(head: Int, tail: List[Int]): Unit = {
    if (head >= max || lengths(head) != 0) tail.foreach(n => lengths(n) = -1)
    else {
      val length = tail.indexOf(head) + 1
      if (length > 0) {
        tail.take(length).foreach(n => lengths(n) = length)
        tail.drop(length).foreach(n => lengths(n) = -1)
      }
      else sociableNums(sumOfDivisers(head), head :: tail)
    }
  }

  def solution = {
    (1 until max).foreach(n => sociableNums(n, Nil))
    val biggest = lengths.max
    lengths.indexWhere(_ == biggest)
  }

  println(solution)
}
