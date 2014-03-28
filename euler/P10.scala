package nlp.scala.euler

object P10 extends App {
  def primeNumbers(n: Int) = {
    val primes = Array.fill(n)(true)
    for {
      prime <- 2 to (n - 1)
      if primes(prime)
      multi <- (prime * 2) to (n - 1) by prime
    } {
      primes(multi) = false
    }

    (2 to (n - 1)).filter(primes(_))
  }

  println(primeNumbers(2000000).map(_.toLong).sum)
}