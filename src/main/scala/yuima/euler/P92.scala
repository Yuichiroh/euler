package yuima.euler

import scala.collection.mutable.Map
import scala.language.postfixOps

/** Square digit chains
  * Problem 92
  * A number chain is created by continuously adding the square of the digits in a number to form a new number until it has been seen before.
  *
  * For example,
  *
  * 44 → 32 → 13 → 10 → 1 → 1
  * 85 → 89 → 145 → 42 → 20 → 4 → 16 → 37 → 58 → 89
  *
  * Therefore any chain that arrives at 1 or 89 will become stuck in an endless loop.
  * What is most amazing is that EVERY starting number will eventually arrive at 1 or 89.
  *
  * How many starting numbers below ten million will arrive at 89?
  */
object P92 extends App {
  val numDigits = if(args.length > 0) args(0).toInt else 7
  val squares = (0 to 9).map(n => n -> n * n).toMap
  val memo = Map[Int, Int]()

  memo(0) = 0
  memo(1) = 1
  memo(89) = 89

  val nums = (0 to 9).flatMap(Seq.fill(numDigits)(_)).combinations(numDigits)

  def enumerationSize(num: IndexedSeq[Int]) = (numDigits !) / (0 to 9).map(i => num.count(_ == i) !).product

  def next(num: IndexedSeq[Int]) = num.map(squares(_)).sum.digits.toIndexedSeq.sorted

  def oneOr89(num: IndexedSeq[Int]): Int = memo.getOrElseUpdate(num.mkString.toInt, oneOr89(next(num)))

  def solution = math.pow(10, numDigits).toInt - nums.withFilter(n => oneOr89(n) != 89).map(enumerationSize).sum

  println(solution)
}
