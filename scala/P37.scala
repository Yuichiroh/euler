package euler.scala

import scala.collection.mutable

import scala.collection.immutable.Stream.consWrapper

object P37 extends App {
  /**
   * Truncatable primes
   * Problem 37
   *
   * The number 3797 has an interesting property.
   * Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7.
   * Similarly we can work from right to left: 3797, 379, 37, and 3.
   *
   * Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
   * NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
   */

  /**
   * 右から順に素数になる数だけを生成していって、最後に左を消して確かめるものと、
   *  左から順に素数になる数だけを生成していって、最後に右を消して確かめるものを両方試す。
   *  最初の素数の生成は、進むに連れて左から順に生成するもののほうがバリエーションが少なくなるので、
   *  おそらく左からのほうが早く収束する。
   */

  val memoPrimes = mutable.Map.empty[Long, Boolean]

  def isPrime(n: Long) = {
    memoPrimes.get(n) match {
      case Some(p) => p
      case None => {
        if (Iterator.from(2).takeWhile(m => m * m <= n).forall(n % _ != 0)) { memoPrimes.put(n, true); true }
        else { memoPrimes.put(n, false); false }
      }
    }
  }

  val lPrimitives = Stream(2, 3, 5, 7L)
  val rPrimitives = Stream(3, 7L)

  def l2r(prime: Long): Stream[Long] = Stream(1, 3, 7, 9).map(n => (prime * 10) + n)
  def r2l(prime: Long): Stream[Long] = Stream(1, 2, 3, 5, 7, 9).map(n => prime + (Math.pow(10, prime.toString.size).toLong * n))

  def r2lPrimes(primes: Stream[Long]): Stream[Long] = {
    val ps = primes.flatMap(r2l).filter(isPrime)
    ps match {
      case Stream() => ps
      case _ => ps #::: r2lPrimes(ps)
    }
  }

  def l2rPrimes(primes: Stream[Long]): Stream[Long] = {
    val ps = primes.flatMap(l2r).filter(isPrime)
    ps match {
      case Stream() => ps
      case _ => ps #::: l2rPrimes(ps)
    }
  }

  val memoL2RTruncatablePrimes = mutable.Map.empty[Long, Boolean]
  val memoR2LTruncatablePrimes = mutable.Map.empty[Long, Boolean]

  def isL2RTruncatable(n: Long): Boolean = {
    memoL2RTruncatablePrimes.get(n) match {
      case Some(p) => p
      case None => {
        if (n < 10L) {
          if (rPrimitives.contains(n)) { memoL2RTruncatablePrimes.put(n, true); true }
          else false
        }
        else if (isPrime(n)) {
          if (isL2RTruncatable(n % (Math.pow(10, (n.toString.size - 1)).toInt).toLong)) { memoL2RTruncatablePrimes.put(n, true); true }
          else { memoL2RTruncatablePrimes.put(n, false); false }
        }
        else { memoL2RTruncatablePrimes.put(n, false); false }
      }
    }
  }

  def isR2LTruncatable(n: Long): Boolean = {
    memoR2LTruncatablePrimes.get(n) match {
      case Some(p) => p
      case None => {
        if (n < 10L) {
          if (lPrimitives.contains(n)) { memoR2LTruncatablePrimes.put(n, true); true }
          else false
        }
        else if (isPrime(n)) {
          if (isR2LTruncatable(n / 10L)) { memoR2LTruncatablePrimes.put(n, true); true }
          else { memoR2LTruncatablePrimes.put(n, false); false }
        }
        else { memoR2LTruncatablePrimes.put(n, false); false }
      }
    }
  }

  /** l2r は手早く終息する */
  def solution0 = {
    //    l2rPrimes(lPrimitives).withFilter(isL2RTruncatable).foreach(println)
    l2rPrimes(lPrimitives).filter(isL2RTruncatable).sum
  }

  /** r2l は経済的な時間で収束しない */
  def solution1 = {
    //    r2lPrimes(rPrimitives).withFilter(isR2LTruncatable).foreach(println)
    r2lPrimes(rPrimitives).filter(isR2LTruncatable).sum
  }

  val sId = if (args.size > 0) args(0).toInt else 0
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}