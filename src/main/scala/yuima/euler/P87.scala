package yuima.euler

/**
 * Prime power triples
 * Problem 87
 * The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28.
 * In fact, there are exactly four numbers below fifty that can be expressed in such a way:
 *
 * 28 = 2^2 + 2^3 + 2^4
 * 33 = 3^2 + 23 + 2^4
 * 49 = 5^2 + 23 + 24
 * 47 = 2^2 + 3^3 + 2^4
 *
 * How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?
 **/
object P87 extends App {
  val max = args(0).toInt
  val primes = primeNumbers(math.sqrt(max).toInt)

  val square = (n: Int) => math.pow(n, 2).toInt
  val cube = (n: Int) => math.pow(n, 3).toInt
  val fourth = (n: Int) => math.pow(n, 4).toInt

  val numbers = for {
    s <- primes.map(square)
    c <- primes.toIterator.map(cube).takeWhile(_ < max - s)
    f <- primes.toIterator.map(fourth).takeWhile(_ < max - s - c)
  } yield s + f + c

  println(numbers.distinct.size)
}
