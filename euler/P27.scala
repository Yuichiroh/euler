package nlp.scala.euler

object P27 extends App {
  /*
   * Euler discovered the remarkable quadratic formula:
   * 
   * n² + n + 41
   * 
   * It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. 
   * However, when n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly divisible by 41.
   * 
   * The incredible formula  n² − 79n + 1601 was discovered, which produces 80 primes for the consecutive values n = 0 to 79. 
   * The product of the coefficients, −79 and 1601, is −126479.
   * 
   * Considering quadratics of the form:
   * 
   * n² + an + b, where |a| < 1000 and |b| < 1000
   * 
   * where |n| is the modulus/absolute value of n
   * e.g. |11| = 11 and |−4| = 4
   * Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of primes for consecutive values of n, starting with n = 0.
   *  */

  val primes = collection.mutable.Map[Long, Boolean]()
  def isPrime(num: Long) = if (num < 2) false else primes.getOrElseUpdate(num, Iterator.from(2).takeWhile(n => n * n <= num).forall(num % _ != 0))

  val bound = 999
  val lengths = for (a <- (-1 * bound to bound); b <- (-1 * bound to bound)) yield {
    val length = Iterator.from(0).takeWhile(n => isPrime(n.toLong * n + a * n.toLong + b)).length
    (length, a, b)
  }
  val maxLengthPair = lengths.max
  println(maxLengthPair._2 * maxLengthPair._3)

  println(maxLengthPair)

  //  (1 to 100).foreach(n => println((n, isPrime(n))))
}