package nlp.scala.euler

object P21 extends App {
  /**
   * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
   * If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
   *
   * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
   *
   * Evaluate the sum of all the amicable numbers under 10000.
   */
  def factorize(num: Long, prime: Long = 2L): List[Long] = num match {
    case 1 => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0 => prime :: factorize(num / prime, prime)
    case _ => factorize(num, prime + 1)
  }

  def sumOfDivisers(num: Int) = factorize(num).groupBy(prime => prime).map(e => (0 to e._2.length).map(k => BigInt(e._1).pow(k)).sum.toInt).product - num

  val answer = (2 until 10000).filter(n => { val m = sumOfDivisers(n); m < 10000 && n != m && n == sumOfDivisers(m) })
  println(answer.sum)
  println(answer.toString)
}