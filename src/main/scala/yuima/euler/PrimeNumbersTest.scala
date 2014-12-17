package yuima.euler

object PrimeNumbersTest extends App {
  def sieve(xs: Stream[Int]): Stream[Int] = xs.head #:: sieve(xs.tail.filter(_ % xs.head != 0))
  def primes = sieve(Stream.from(2))

  def primeNumbersArray(index: Int) = {
    val primes = new Array[Int](index)
    primes(0) = 2
    for (i <- (1 to index - 1)) {
      primes(i) = Iterator.from(primes(i - 1) + 1).withFilter(m =>
        (0 to i - 1).forall(m % primes(_) != 0)).next
    }
    primes
  }

  val index = if(args.size > 1) args(1).toInt else 1000
  val sId = if (args.size > 0) args(0).toInt else 0
  def solution = sId match {
    case 0 => primes(index)
    case 1 => primeNumbersArray(index+1)(index)
    case 2 => new Primes(100000)(index)
  }
  println(solution)
}