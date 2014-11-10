package euler

import scala.collection.mutable

/** Coin partitions
  * Problem 78
  * Let p(n) represent the number of different ways in which n coins can be separated into piles.
  * For example, five coins can separated into piles in exactly seven different ways, so p(5)=7.
  *
  * OOOOO
  * OOOO   O
  * OOO   OO
  * OOO   O   O
  * OO   OO   O
  * OO   O   O   O
  * O   O   O   O   O
  * Find the least value of n for which p(n) is divisible by one million.
  */
object P78 extends App {
  val condition = if (args.size > 0) args(0).toInt else 1000000

  /** !!! this does not terminate in a short time period. !!! */
  def solution0 = {
    case class NumWithSplitMax(n: Int, splitMax: Int)

    val combinations = mutable.Map[NumWithSplitMax, Int]()

    def combination(num: NumWithSplitMax): Int =
      if (num.n == 1) 1
      else if (num.splitMax == 1) 1
      else combinations.getOrElseUpdate(num,
        rightTermsOfEverySplit(num).map(combination(_)).sum + (if (num.n <= num.splitMax) 1 else 0)
      )

    def rightTermsOfEverySplit(num: NumWithSplitMax) =
      (1 until (num.n min (num.splitMax + 1))).map(n => NumWithSplitMax(num.n - n, n))

    Iterator.from(1).dropWhile { n => combination(NumWithSplitMax(n, n - 1)) % condition != 0 }.next()
  }

  /** Partition function
    * http://www.wikiwand.com/en/Partition_(number_theory)#/Partition_function
    */
  def solution1 = {
    val ps = mutable.Map[Int, Int]()
    ps(0) = 1
    ps(1) = 1

    val signs = Seq(1, 1, -1, -1)

    def sign(m: Int) = signs(m % 4)

    val generalizedPentagonalNumbers = Stream.from(1).flatMap(m => Stream(m, -m)).map(m => (3 * m - 1) * m / 2)

    def p(n: Int): Int = ps.getOrElseUpdate(n,
      Iterator.from(0).map(i => (sign(i), n - generalizedPentagonalNumbers(i)))
        .takeWhile(_._2 >= 0).map(k => k._1 * p(k._2)).sum % condition
    )

    Iterator.from(2).dropWhile(n => p(n) % condition != 0).next()
  }

  /** Efficient implementation of the Hardy–Ramanujan–Rademacher formula
    * see http://journals.cambridge.org/action/displayAbstract?fromPage=online&aid=8710297
    */
  def solution2 = {
    //TODO: implementation
  }

  println(solution1)
}