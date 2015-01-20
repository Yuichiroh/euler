package yuima.euler

import scala.util.Random

object P58 extends App {
  /**
   * Spiral primes
   * Problem 58
   * Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.
   *
   * 37 36 35 34 33 32 31
   * 38 17 16 15 14 13 30
   * 39 18  5  4  3 12 29
   * 40 19  6  1  2 11 28
   * 41 20  7  8  9 10 27
   * 42 21 22 23 24 25 26
   * 43 44 45 46 47 48 49
   *
   * It is interesting to note that the odd squares lie along the bottom right diagonal,
   * but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.
   *
   * If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed.
   * If this process is continued, what is the side length of the square spiral
   * for which the ratio of primes along both diagonals first falls below 10%?
   */

  /**
   * 辺の長さ2k-1のときの四つ角の数：1, 3-5-7-9, 13-17-21-25, 31-37-43-49, ... : 初期値はa_k、差は2(k -1)、最大値はa_k + 6(k-1)、次の最小値はa_k + 6(k-1) + 2k
   */

  def corners(k: Int = 1, a: Int = 1): Stream[List[Int]] = List(a, a + 2 * (k - 1), a + 4 * (k - 1), a + 6 * (k - 1)) #:: corners(k + 1, a + 8 * k - 6)

  var numPrime = 0
  var numCorner = 0

  def numDiagonalPrimes(primeTest: Int => Boolean) = corners(2, 3).takeWhile(ns => {
    val ps = ns.count(primeTest)
    numPrime += ps
    numCorner += 4
    numPrime.toDouble / numCorner >= 0.1
  }).size * 2 + 1

  def isPrime(n: Int) = Iterator.from(2).takeWhile(m => m * m <= n).forall(n % _ != 0)

  def isPrimeMillerRabin(iter: Int)(n: Int) = {
    if (n == 2) true
    else if (n % 2 == 0) false
    else if (n < 9) true
    else if (n % 3 == 0) false
    else {
      val d = divBy2(n - 1)
      Iterator.fill(iter)(Random.nextInt(n - 1) + 1).forall(a => {
        var t = d
        var y = pow(a, t, n, 1L)
        while ((t != n - 1) && (y != 1) && (y != n - 1)) {
          y = (y * y) % n
          t <<= 1
        }
        y == n - 1 || (t & 1) != 0
      })
    }
  }

  def divBy2(n: Int): Int = if (n % 2 != 0) n else divBy2(n / 2)

  def pow(base: Long, power: Int, mod: Int, result: Long): Long = {
    if (power > 0) pow((base * base) % mod, power >> 1, mod, if ((power & 1) == 1) (result * base) % mod else result)
    else result
  }
  
  def solution0 = numDiagonalPrimes(isPrime)

  def solution1 = {
    val primes = new Primes(10e2.toInt)
    numDiagonalPrimes(primes.isPrime)
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}