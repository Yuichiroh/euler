package euler.scala

object P46 extends App {
  /**
   * Goldbach's other conjecture
   * Problem 46
   * It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.
   *
   * 9 = 7 + 2×1^2
   * 15 = 7 + 2×2^2
   * 21 = 3 + 2×3^2
   * 25 = 7 + 2×3^2
   * 27 = 19 + 2×2^2
   * 33 = 31 + 2×1^2
   *
   * It turns out that the conjecture was false.
   *
   * What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?
   */

  val memoPrimes = collection.mutable.Map[Long, Boolean]()

  def isPrime(num: Int) = if (num < 2) false else memoPrimes.getOrElseUpdate(num,
    Iterator.from(2).takeWhile(n => n * n <= num).forall(num % _ != 0))

  val primes = Stream.from(2).filter(isPrime)

  val solution0 = Iterator.from(2).map(_ * 2 - 1).withFilter(!isPrime(_)).withFilter(n =>
    !primes.takeWhile(_ < n).exists(p => Math.sqrt((n - p) / 2.0).isValidInt)
  ).next

  println(solution0)
}