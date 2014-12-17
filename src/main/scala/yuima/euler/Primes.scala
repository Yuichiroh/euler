package yuima.euler

import scala.collection.mutable.ArrayBuffer

object Primes {
  def apply(sieveMax: Int) = new Primes(sieveMax)
}

class Primes(val sieveMax: Int) extends ArrayBuffer[Int] {
  private[this] val sieve = new ArrayBuffer[Int]()
  private[this] val memoIsPrime = scala.collection.mutable.Map[Long, Boolean]()
  private[this] var current = 0

  append(2)
  sieve.append(-1, -1, 0)

  override def apply(i: Int) = if (i < length) super.apply(i) else expandPrimes(i)

  def index(n: Int) = if (n < sieve.length) sieve(n) else expandSieve(n)

  def isPrime(n: Int) = if (n < sieveMax) isPrimeWithSieve(n) else isPrimeWithMillerRabin(n)

  def isPrime(n: Long) = if (n < sieveMax) isPrimeWithSieve(n.toInt) else isPrimeWithMillerRabin(n)

  def isPrimeWithSieve(n: Int) = if (index(n) == -1) false else true

  def next = {
    current += 1
    apply(current - 1)
  }

  private[this] def expandPrimes(i: Int) = {
    for (j <- length to i if sieve.length < sieveMax) append(
      Iterator.from(this(j - 1) + 1).withFilter(n => {
        val result = (0 to j - 1).forall(n % this(_) != 0)
        if (sieve.length <= n) if (result) sieve.append(j) else sieve.append(-1)
        result
      }).next
    )
    if (i < length) this(i) else -1
  }

  def expandSieve(n: Int) = {
//    println("expanding to", n)
    for (m <- sieve.length to n if m < sieveMax) sieve.append({
      val result = takeWhile(p => p * p <= m).forall(m % _ != 0)
      if (result) { append(m); length } else -1
    })
    if (n < sieveMax) sieve(n) else -1
  }

  /** This function is safe until the smallest strong pseudo prime for (2,3,5,7), which is: 3,215,031,751.
    * See the paper for the smallest strong pseudo prime with first m prime bases. http://arxiv.org/pdf/1207.0063v1.pdf
    */
  def isPrimeWithMillerRabin(n: Long) = {
    def divBy2(n: Long): Long = if (n % 2 != 0) n else divBy2(n / 2)

    def pow(base: Long, power: Long, mod: Long, result: Long): Long = {
      if (power > 0) pow((base * base) % mod, power >> 1, mod, if ((power & 1) == 1) (result * base) % mod else result)
      else result
    }

    memoIsPrime.getOrElseUpdate(n, {
      val d = divBy2(n - 1)
      List(2, 3, 5, 7).forall(a => {
        var t = d
        var y = pow(a, t, n, 1L)
        while ((t != n - 1) && (y != 1) && (y != n - 1)) {
          y = (y * y) % n
          t <<= 1
        }
        y == n - 1 || (t & 1) != 0
      })
    })
  }
}