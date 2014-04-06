package euler.scala

object P41 extends App {
  /**
   * Pandigital prime
   * Problem 41
   * We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once.
   * For example, 2143 is a 4-digit pandigital and is also prime.
   *
   * What is the largest n-digit pandigital prime that exists?
   */

  def isPrime(n: Int) = Iterator.from(2).takeWhile(m => m * m <= n).forall(n % _ != 0)
  implicit def seq2int(num: Seq[Int]) = (0 /: num) { (s, i) => s * 10 + i }

  /** パンデジタル数が素数になるのは、７桁と４桁の時だけ。（各桁の和が３の倍数になるので） */
  def solution0 = ((7 to 1 by -1).permutations ++ (4 to 1 by -1).permutations).map(seq2int).withFilter(isPrime).next

  println(solution0)
}