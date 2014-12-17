package yuima.euler

import scala.collection.mutable

/** Prime summations
  * Problem 77
  * It is possible to write ten as the sum of primes in exactly five different ways:
  *
  * 7 + 3
  * 5 + 5
  * 5 + 3 + 2
  * 3 + 3 + 2 + 2
  * 2 + 2 + 2 + 2 + 2
  *
  * What is the first value which can be written as the sum of primes in over five thousand different ways?
  */
object P77 extends App {
  val condition = if (args.size > 0) args(0).toInt else 5000

  def solution = {
    case class NumWithSplitMax(n: Int, splitMax: Int)

    val primes = Primes(10000)

    val combinations = mutable.Map[NumWithSplitMax, Int]()

    def combination(num: NumWithSplitMax): Int =
      if (num.n == 1) 0
      else if (num.splitMax == 1) 1
      else combinations.getOrElseUpdate(num,
        rightTermsOfEverySplit(num).map(combination(_)).sum + (if (num.n <= num.splitMax && primes.isPrime(num.n)) 1 else 0)
      )

    def rightTermsOfEverySplit(num: NumWithSplitMax) =
      Iterator.from(0).map(primes(_)).takeWhile(p => p < (num.n min (num.splitMax + 1))).map(p => NumWithSplitMax(num.n - p, p)).toList

    Iterator.from(3).dropWhile { n => combination(NumWithSplitMax(n, n - 1)) < condition }.next()
  }

  println(solution)

}