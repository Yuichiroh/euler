package yuima.euler

/** Totient maximum
  * Problem 69
  * Euler's Totient function, φ(n) [sometimes called the phi function],
  * is used to determine the number of numbers less than n which are relatively prime to n.
  * For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
  *
  * n	Relatively Prime	φ(n)	n/φ(n)
  * 2	1	1	2
  * 3	1,2	2	1.5
  * 4	1,3	2	2
  * 5	1,2,3,4	4	1.25
  * 6	1,5	2	3
  * 7	1,2,3,4,5,6	6	1.1666...
  * 8	1,3,5,7	4	2
  * 9	1,2,4,5,7,8	6	1.5
  * 10	1,3,7,9	4	2.5
  * It can be seen that n=6 produces a maximum n/φ(n) for n ≤ 10.
  *
  * Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.
  */
object P69 extends App {
  /** Euler's totient function: see Wikipedia http://www.wikiwand.com/en/Euler%27s_totient_function
    * phi(n) = n * product(1 - 1/p_k) using n's prime factors ( n = product_k((p_k)^(e_k)) )
    */
  def solution0 = {
    val factorizer = Factorization(limit)
    def phi(n: Int) = factorizer.factorize(n).map(f => 1 - 1.0 / f.p).product.toDouble * n

    (1 to limit).map(n => (n / phi(n), n)).maxBy(_._1)
  }

  /** n/phi(n) = product(p_k/(p_k - 1)) where each term is bigger than 1.
    * So the largest n/phi(n) on the condition n < N is obtained by n which is the product of all the primes less than N.
    */
  def solution1 = {
    val primes = Primes(limit)
    Iterator.iterate(1)(x => x * primes.next).takeWhile(_ <= limit).max
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  val limit = args(1).toInt
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}