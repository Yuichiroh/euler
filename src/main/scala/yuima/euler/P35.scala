package yuima.euler

import scala.collection.mutable

import scala.collection.immutable.Stream.consWrapper

object P35 extends App {
  /**
   * Circular primes
   * Problem 35
   * The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.
   *
   * There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.
   *
   * How many circular primes are there below one million?
   */

  val primes = mutable.Map[Long, Boolean]()
  val circularPrimes = mutable.Map[Int, Boolean]()
  val maxDigitSize = 6 // 1,000,000 - 1 は６桁

  def circulars(num: Seq[Int]): Stream[Seq[Int]] = {
    val next = (num.head +: num.tail.reverse).reverse
    next #:: circulars(next)
  }

  def isPrime(num: Int) =
    if (num < 2) false
    else primes.getOrElseUpdate(
      num,
      Iterator.from(2).takeWhile(n => n * n <= num).forall(num % _ != 0)
    )

  def isCircularPrime(num: Seq[Int]) = circularPrimes.getOrElseUpdate(
    num.mkString("").toInt,
    (num #:: circulars(num).takeWhile(!_.equals(num))).map(_.mkString.toInt).forall(isPrime)
  )

  /*７以上の素数は、下一桁が1,3,7,9でなければならない。さらに循環素数であるためには、各桁が1,3,7,9でなければならない。*/
  def solution0 = {
    (2 to maxDigitSize).flatMap(n =>
      Seq(1, 3, 7, 9)
        .flatMap(Seq.fill(n)(_)).combinations(n)
        .flatMap(_.permutations)
    ).count(isCircularPrime) + 4 // 2,3,5,7を足す
  }

  println(solution0)
}