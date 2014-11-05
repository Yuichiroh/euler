package euler

import scala.collection.mutable

object P31 extends App {
  /**
   * In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:
   * 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
   *
   * It is possible to make £2 in the following way:
   * 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
   *
   * How many different ways can £2 be made using any number of coins?
   */

  val sum = if(args.size > 0) args(0).toInt else 200
  val coins = List(1, 2, 5, 10, 20, 50, 100, 200)

  /** using generating function */
  def generatingFunc(n: Int) = Iterator.from(0).map(_ * n -> 1).takeWhile(_._1 <= sum).toMap

  def multi(f1: mutable.Map[Int, Int], f2: Map[Int, Int]) = {
    val newFunc = mutable.Map.empty[Int, Int].withDefaultValue(0)
    for {
      (k1, v1) <- f1
      (k2, v2) <- f2
      if k1 + k2 <= sum
    } newFunc(k1 + k2) += v1 * v2
    newFunc
  }

  val unit = mutable.Map.empty[Int, Int].withDefaultValue(0) + (0 -> 1)
  println((unit /: coins.map(generatingFunc(_))) { (f1, f2) => multi(f1, f2) }(200))
} 