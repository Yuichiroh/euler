package yuima.euler

/**
 * Test it
 * @author Yuichiroh Matsubayashi
 *         Created on 15/01/01.
 */
object PrimeOutInYears extends App {
  val primeOutInYears = (1 to 3000).filter(year => isPrimes(yearOutAndIn(year)))

  def yearOutAndIn(year: Long) = Seq(year * 10000 + 1231, (year + 1) * 10000 + 101)

  def isPrimes(ns: Seq[Long]) = ns.forall(isPrimeMillerRabin)

  println((2000 to 2010).map(yearOutAndIn(_)))
  println(primeOutInYears)
}
