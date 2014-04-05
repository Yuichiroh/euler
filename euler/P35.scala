package euler.scala

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

  val memoPrimes = mutable.Map.empty[Int, Int].withDefaultValue(0)
  val memoCircularPrimes = mutable.Map.empty[Int, Int].withDefaultValue(0)
  val max = 6 // 1,000,000 - 1 は６桁

  def circulars(num: List[Int]): Stream[List[Int]] = {
    val next = (num.head :: num.tail.reverse).reverse
    next #:: circulars(next)
  }

  def isPrime(n: Int) = {
    memoPrimes.get(n) match {
      case None => {
        if (Iterator.from(2).takeWhile(m => m * m <= n).forall(n % _ != 0)) {
          memoPrimes.put(n, 1); true
        }
        else {
          memoPrimes.put(n, 1); false
        }
      }
      case Some(-1) => false
      case _ => true
    }
  }

  def isCircularPrime(num: List[Int]) = {
    memoCircularPrimes.get(num.mkString("").toInt) match {
      case None => {
        val candidates = (num #:: circulars(num).takeWhile(!_.equals(num))).map(_.mkString("").toInt)
        if (candidates.forall(isPrime)) {
          candidates.foreach(n => memoCircularPrimes.put(n, 1)); true
        }
        else {
          candidates.foreach(n => memoCircularPrimes.put(n, -1)); false
        }
      }
      case Some(-1) => false
      case _ => true
    }
  }

  /*７以上の素数は、下一桁が1,3,7,9でなければならない。さらに循環素数であるためには、各桁が1,3,7,9でなければならない。*/
  def solution0 = {
    val nums = (2 to max).flatMap(n =>
      List(1, 3, 7, 9).flatMap(List.fill(n)(_)).combinations(n).
        flatMap(_.permutations.toList)
    )
    val circularPrimes = for {
      num <- nums
      if isCircularPrime(num)
    } yield num.mkString("").toInt
    circularPrimes.length + 4 // 2,3,5,7を足す
  }

  println(solution0)
}