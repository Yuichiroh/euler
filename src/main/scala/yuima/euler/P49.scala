package yuima.euler

object P49 extends App {
  /**
   * Prime permutations
   * Problem 49
   * The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.
   *
   * There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.
   *
   * What 12-digit number do you form by concatenating the three terms in this sequence?
   */

  val min = 1000
  val max = 10000

  val isPrime = {
    val primes = Array.fill(max)(true)
    for {
      prime <- Iterator.from(2).takeWhile(n => n * n < max - 1).filter(primes)
      multi <- (prime * 2) to (max - 1) by prime
    } primes(multi) = false
    primes
  }

  def primeNumbers = (min to (max - 1)).filter(isPrime)

  def solution0 = primeNumbers.groupBy(_.toString.toSeq.sorted).withFilter(_._2.size > 2).
    flatMap(_._2.combinations(3).filter(e => (e(1) - e(0) == e(2) - e(1))))

  println(solution0)
}