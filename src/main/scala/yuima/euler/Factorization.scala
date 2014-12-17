package yuima.euler

class Factorization(max: Int) {
  private val factors = (0 to max).toArray
  //    factors(0) = 0

  for {
    prime <- Iterator.from(2).takeWhile(p => p * p < max) if factors(prime) == prime
    k <- (prime * 2) to max by prime
  } factors(k) = prime

  def factorize(n: Int): List[Factor] = n match {
    case 1 => Nil
    case _ =>
      val p = factors(n)
      val (e, m) = divPow(n, p)
      Factor(p, e) :: factorize(m)
  }

  def flatFactorize(n: Int): List[Int] = n match {
    case 1 => Nil
    case _ =>
      val p = factors(n)
      val (e, m) = divPow(n, p)
      List.fill(e)(p) ::: flatFactorize(m)
  }

  def divPow(n: Int, divisor: Int, e: Int = 0): (Int, Int) = (n % divisor) match {
    case 0 => divPow(n / divisor, divisor, e + 1)
    case _ => (e, n)
  }
}

object Factorization {
  def apply(n: Int) = new Factorization(n)
}

case class Factor(p: Int, e: Int)