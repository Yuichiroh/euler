package yuima.euler

/**
 * Enumerates years that the last date of the year and first date of the next year are primes.
 * @author Yuichiroh Matsubayashi
 *         Created on 15/01/01.
 */
object PrimeOutInYears extends App {
  val bound = args(0).toLong

  val primeOutInYears = (1L to bound).filter(year => isPrimes(yearOutAndIn(year)))

  def yearOutAndIn(year: Long) = Seq(year * 10000 + 1231, (year + 1) * 10000 + 101)

  def isPrimes(ns: Seq[Long]) = ns.forall(isPrimeMillerRabin)

  println(primeOutInYears)
}
