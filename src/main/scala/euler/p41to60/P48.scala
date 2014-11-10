package euler

object P48 extends App {
  /**
   * Self powers
   * Problem 48
   * The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
   *
   * Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
   */

  def solution0 = Iterator.from(1).map(n => BigInt(n).pow(n)).take(1000).sum.toString.takeRight(10)

  /** 下10桁のみ保持しておけば良い */
  def solution1 = (0L /: (1 to 1000)) { (sum, n) => (sum + pow(n, n)) % 10000000000L }

  def pow(n: Int, m: Int): Long = m match {
    case 0 => 1L
    case _ => (n * pow(n, m - 1)) % 10000000000L
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}