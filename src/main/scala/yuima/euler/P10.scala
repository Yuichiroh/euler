package yuima.euler

object P10 extends App {
  /**
   * Summation of primes
   * Problem 10
   * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
   *
   * Find the sum of all the primes below two million.
   */

  def primeNumbers(max: Int) = {
    val primes = Array.fill(max)(true)
    for {
      prime <- Iterator.from(2).takeWhile(n => n * n < max - 1).filter(primes)
      multi <- (prime * 2) to (max - 1) by prime
    } primes(multi) = false
    (2 to (max - 1)).filter(primes(_))
  }

  def solution0 = primeNumbers(2000000).map(_.toLong).sum
  
  println(solution0)
}