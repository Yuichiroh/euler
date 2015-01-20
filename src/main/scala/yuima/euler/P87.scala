package yuima.euler

/** Prime power triples
  * Problem 87
  *
  * The smallest number expressible as the sum of a prime square, prime cube, and prime fourth power is 28.
  * In fact, there are exactly four numbers below fifty that can be expressed in such a way:
  *
  * 28 = 2^2 + 2^3 + 2^4
  * 33 = 3^2 + 2^3 + 2^4
  * 49 = 5^2 + 2^3 + 2^4
  * 47 = 2^2 + 3^3 + 2^4
  *
  * How many numbers below fifty million can be expressed as the sum of a prime square, prime cube, and prime fourth power?
  * */
object P87 extends App {
  val max = if (args.length > 0 ) args(0).toInt else 50000000
  val primes = primeNumbers(math.sqrt(max).toInt).toArray

  val squares = primes.map(n => math.pow(n, 2).toInt)
  val cubes = primes.toStream.map(n => math.pow(n, 3).toInt)
  val fourths = primes.toStream.map(n => math.pow(n, 4).toInt)

  val numbers = for {
    s <- squares
    c <- cubes.takeWhile(_ < max - s)
    f <- fourths.takeWhile(_ < max - s - c)
  } yield s + f + c

  println(numbers.distinct.size)
}
