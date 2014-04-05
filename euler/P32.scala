package euler.scala

object P32 extends App {
  /**
   * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.
   * The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
   * Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
   *
   * HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.
   */

  def list2int(num: List[Int]) = (0 /: num) { (s, i) => s * 10 + i }

  def solution0 = {
    val nums = (1 to 9).toList.permutations
    nums.withFilter(_ match {
      case a :: b :: c :: d :: e :: rest =>
        (a * 10 + b) * (c * 100 + d * 10 + e) == list2int(rest) ||
          a * (b * 1000 + c * 100 + d * 10 + e) == list2int(rest)
      case _ => false
    }).map(num => list2int(num.takeRight(4))).toList.distinct.sum
  }

  println(solution0)
}