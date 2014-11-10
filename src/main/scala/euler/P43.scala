package euler

import scala.language.implicitConversions
import scala.language.postfixOps

object P43 extends App {
  /**
   * Sub-string divisibility
   * Problem 43
   * The number, 1406357289, is a 0 to 9 pandigital number because it is made up of each of the digits 0 to 9 in some order, but it also has a rather interesting sub-string divisibility property.
   *
   * Let d1 be the 1st digit, d2 be the 2nd digit, and so on. In this way, we note the following:
   *
   * d2d3d4=406 is divisible by 2
   * d3d4d5=063 is divisible by 3
   * d4d5d6=635 is divisible by 5
   * d5d6d7=357 is divisible by 7
   * d6d7d8=572 is divisible by 11
   * d7d8d9=728 is divisible by 13
   * d8d9d10=289 is divisible by 17
   * Find the sum of all 0 to 9 pandigital numbers with this property.
   */

  def solution0 = {
    val divisors = List(2, 3, 5, 7, 11, 13, 17)
    val pandigitals = (0 to 9).permutations

    def constraints(n: Seq[Int]) = n(5) == 5
    def isAllDivisible(n: Seq[Int]) = (0 to 6).forall(k => n.slice(k + 1, k + 4).mkString.toInt % divisors(k) == 0)

    pandigitals.collect { case n if constraints(n) && isAllDivisible(n) => n.mkString.toLong }.sum
  }

  def solution1 = {
    implicit def int2list(n: Int) = n.toString.map(_.toString.toInt).toList
    implicit def list2int(num: List[Int]) = (0L /: num) { (s, i) => s * 10 + i }

    def multiples(n: Int) = Iterator.from(1).map(n*).map(int2list).withFilter(_.size > 1).
      map(m => if (m.size < 3) 0 :: m else m).withFilter(m => m == m.distinct).takeWhile(_.size < 4)

    def multi(n: Int, used: List[Int]) = (0 to 9).withFilter(!used.contains(_)).map(_ :: used).filter(_.slice(0, 3) % n == 0)

    (multiples(17) /: Seq(13, 11, 7, 5, 3, 2, 1))((lists, n) => lists.flatMap(m => multi(n, m))).map(list2int).sum
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}