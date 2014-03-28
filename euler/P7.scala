package nlp.scala.euler

object P7 extends App {
  def primes: Stream[Int] = 2 #:: Stream.from(3).filter(num => primes.takeWhile(prime => prime * prime <= num).forall(num % _ > 0))
  println(primes(10000))
}