package euler

object P47 extends App {
  /**
   * Distinct primes factors
   * Problem 47
   * The first two consecutive numbers to have two distinct prime factors are:
   *
   * 14 = 2 × 7
   * 15 = 3 × 5
   *
   * The first three consecutive numbers to have three distinct prime factors are:
   *
   * 644 = 2² × 7 × 23
   * 645 = 3 × 5 × 43
   * 646 = 2 × 17 × 19.
   *
   * Find the first four consecutive integers to have four distinct prime factors. What is the first of these numbers?
   */

  def factorize(num: Int, prime: Int = 2): List[Int] = num match {
    case 1 => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0 => prime :: factorize(num / prime, prime)
    case _ => factorize(num, prime + 1)
  }

  def takeConsecutives(n: Int, tmp: Int, first: Int, nums: Iterator[Int]): Int = {
    val last = nums.next
    if (tmp < n) {
      if ((last - first - tmp) == 1) takeConsecutives(n, tmp + 1, first, nums)
      else takeConsecutives(n, 0, last, nums)
    }
    else first
  }

  def solution0 = takeConsecutives(3, 0, 210, Iterator.from(211).withFilter(factorize(_).distinct.size > 3))
  println(solution0)
}