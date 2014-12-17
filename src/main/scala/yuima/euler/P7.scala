package yuima.euler

object P7 extends App {
  /**
   * 10001st prime
   * Problem 7
   * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
   *
   * What is the 10 001st prime number?
   */

  def solution0 = primes(target - 1)
  def solution1 = primeNumbers(target).head
  def solution2 = primeNumbersArray(target)(target - 1)

  def primes: Stream[Int] = 2 #:: Stream.from(3).filter(num => primes.takeWhile(prime => prime * prime <= num).forall(num % _ > 0))

  def primeNumbers(n: Int) = (List(2) /: (2 to n)) { (primes, index) =>
    Iterator.from(primes.head + 1).withFilter(m => primes.forall(m % _ != 0)).next :: primes
  }

  def primeNumbersArray(n: Int) = {
    val primes = new Array[Int](n)
    primes(0) = 2
    for (i <- (1 to n - 1)) {
      primes(i) = Iterator.from(primes(i - 1) + 1).withFilter(m =>
        (0 to i - 1).forall(m % primes(_) != 0)).next
    }
    primes
  }

  val target = if (args.size > 1) args(1).toInt else 10001

  val sId = if (args.size > 0) args(0).toInt else 2
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
    case 2 => solution2
  }
  println(solution)
}