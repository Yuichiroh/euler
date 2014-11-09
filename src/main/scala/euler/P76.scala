package euler

import scala.collection.mutable

/** Counting summations
  * Problem 76
  * It is possible to write five as a sum in exactly six different ways:
  *
  * 4 + 1
  * 3 + 2
  * 3 + 1 + 1
  * 2 + 2 + 1
  * 2 + 1 + 1 + 1
  * 1 + 1 + 1 + 1 + 1
  *
  * How many different ways can one hundred be written as a sum of at least two positive integers?
  */
object P76 extends App {
  def solution0 = {
    def multiples(n: Int) = Iterator.from(0).map(_ * n -> 1).takeWhile(_._1 <= sum).toMap

    def combination(f1: mutable.Map[Int, Int], f2: Map[Int, Int]) = {
      val newFunc = mutable.Map.empty[Int, Int].withDefaultValue(0)
      for {
        (k1, v1) <- f1
        (k2, v2) <- f2
        if k1 + k2 <= sum
      } newFunc(k1 + k2) += v1 * v2
      newFunc
    }

    val unit = mutable.Map.empty[Int, Int].withDefaultValue(0) + (0 -> 1)
    val combinations = (unit /: (1 until sum).map(multiples(_))) { (f1, f2) => combination(f1, f2) }
    combinations(sum)
  }

  def solution1 = {
    case class NumWithSplitMax(n: Int, splitMax: Int)

    val combinations = mutable.Map[NumWithSplitMax, Int]()

    def combination(num: NumWithSplitMax): Int = combinations.getOrElseUpdate(num,
      rightTermsOfEverySplit(num).map(combination(_)).sum + (if (num.n > num.splitMax) 0 else 1)
    )

    def rightTermsOfEverySplit(num: NumWithSplitMax) =
      for (i <- (1 to (num.n - 1 min num.splitMax))) yield NumWithSplitMax(num.n - i, i)

    for (i <- 0 until sum) {
      combinations(NumWithSplitMax(i + 1, 1)) = 1
      combinations(NumWithSplitMax(1, i)) = 1
    }
    combination(NumWithSplitMax(sum, sum - 1))
  }

  /** Partition function
    * http://www.wikiwand.com/en/Partition_(number_theory)#/Partition_function
    */
  def solution2 = {
    val ps = mutable.Map[Int, Int]()
    ps(0) = 1
    ps(1) = 1

    val signs = Seq(1, 1, -1, -1)
    val generalizedPentagonalNumbers = Stream.from(1).flatMap(m => Stream(m, -m)).map(m => (3 * m - 1) * m / 2)

    def sign(m: Int) = signs(m % 4)

    def p(n: Int): Int = ps.getOrElseUpdate(n,
      Iterator.from(0).map(i => (sign(i), n - generalizedPentagonalNumbers(i)))
        .takeWhile(_._2 >= 0).map(k => k._1 * p(k._2)).sum
    )

    p(sum) - 1
  }

  val sId = if (args.size > 0) args(0).toInt else 2
  val sum = if (args.size > 1) args(1).toInt else 100

  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
    case 2 => solution2
  }
  println(solution)
}