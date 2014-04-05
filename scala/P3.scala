package euler.scala

object P3 extends App {
  /**
   * Largest prime factor
   * Problem 3
   * The prime factors of 13195 are 5, 7, 13 and 29.
   *
   * What is the largest prime factor of the number 600851475143 ?
   */

  def factorize(num: Long, prime: Long = 2L): List[Long] = num match {
    case 1 => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0 => prime :: factorize(num / prime, prime)
    case _ => factorize(num, prime + 1)
  }

  def solution0 = factorize(600851475143L).reverse.head

  println(solution0)
}