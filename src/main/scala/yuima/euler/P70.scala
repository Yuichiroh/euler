package yuima.euler

import yuima.euler.P69._

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.PriorityQueue
import scala.math.Ordering

/** Totient permutation
  * Problem 70
  * Euler's Totient function, φ(n) [sometimes called the phi function],
  * is used to determine the number of positive numbers less than or equal to n which are relatively prime to n.
  * For example, as 1, 2, 4, 5, 7, and 8, are all less than nine and relatively prime to nine, φ(9)=6.
  * The number 1 is considered to be relatively prime to every positive number, so φ(1)=1.
  *
  * Interestingly, φ(87109)=79180, and it can be seen that 87109 is a permutation of 79180.
  *
  * Find the value of n, 1 < n < 10^7, for which φ(n) is a permutation of n and the ratio n/φ(n) produces a minimum.
  */
object P70 extends App {

  def solution0 = {
    val f = Factorization(limit)
    def phi(n: Int) = f.factorize(n).map(f => 1 - 1.0 / f.p).product.toDouble * n

    def isPermutation(num1: Int, num2: Int) = num1.toString().toCharArray().sorted.mkString == num2.toString().toCharArray().sorted.mkString

    (2 until limit).map(n => (n, phi(n)))
      .filter(pair => isPermutation(pair._1, pair._2.round.toInt))
      .minBy(pair => pair._1 / pair._2)
  }

  /** Find the first number satisfying the condition in the list of n/phi(n) in ascending order.
    * n/phi(n) = product(p_k/(p_k - 1)) where each term is bigger than 1.
    * Therefore, the largest prime under the limit has the smallest n/phi(n),
    * but primes cannot satisfy the condition because phi(p) = p - 1.
    * Except for primes, minimum n/phi(n) is obtained by p^2 where p is the largest prime s.t. p^2 < limit.
    * p^k (k > 2) has the same n/phi(n) as p^2.
    * second smallest n/phi(n) is obtained by
    * (1) p * p_2 where p_2 is the largest prime s.t. p * p_2 < limit or
    * (2) p_2^2 where p_2 is largest prime s.t. p_2 < p.
    *
    * A way for generating a candidates next to n = p_1^{e_1} * ... * p_k^{e_k} (p_i < p_j for i, j s.t. i < j)
    * without producing any duplicated n is following:
    * (1) p_1^{e_1} * ... * p_k^{e_k + 1}
    * (2) if e_k > 1, then p_1^{e_1} * ... * p_k^{e_k - 1} * p where p is the largetst prime > p_k
    * (3) if e_k = 1, then p_1^{e_1} * ... * p_{k-1}^e_{k-1} * p where p is the largest prime s.t. p_{k-1} < p < p_k.
    * (4) if n = p^2, then p'^2 where p' is the largest prime < p
    */
  def solution1 = {
    case class Entry(nPhi: Double, n: Int, factors: List[Factor])
    val ordering = new Ordering[Entry]() {
      def compare(x: Entry, y: Entry) = y.nPhi compare x.nPhi
    }
    val primes = Primes(limit)
    val queue = PriorityQueue[Entry]()(ordering)

    def solve = {
      enqueue(Factor(largestPrimeItsSquereIsLessThan(limit), 2) :: Nil)
      find
    }

    def find: Entry = {
      val candidate = queue.dequeue()
      if (isPermutation(candidate.n, phi(candidate.n, candidate.factors))) candidate
      else {
        enqueueNextCandidates(candidate.factors)
        find
      }
    }

    def isPermutation(num1: Int, num2: Int) = num1.toString().toCharArray().sorted.mkString == num2.toString().toCharArray().sorted.mkString

    def largestPrimeItsSquereIsLessThan(limit: Int) = Iterator.from(math.sqrt(limit).toInt, -1).filter(p => primes.isPrime(p)).next

    def largestPrime(r: Double) = Iterator.from(r.toInt, -1).filter(p => primes.isPrime(p)).next

    def nOf(factors: List[Factor]) = factors.map(f => math.pow(f.p, f.e)).product.toInt

    def phi(n: Int, factors: List[Factor]) = (factors.map(f => 1 - 1.0 / f.p).product.toDouble * n).round.toInt

    def nPhi(factors: List[Factor]) = factors.map(f => f.p.toDouble / (f.p - 1)).product

    def enqueueNextCandidates(factors: List[Factor]) = {
      val head = factors.head
      /** rule 1 */
      enqueue(Factor(head.p, head.e + 1) :: factors.tail)
      /** rule 2 */
      if (head.e > 1) {
        val suffix = Factor(head.p, head.e - 1) :: factors.tail
        val p = largestPrime(limit / nOf(suffix))
        if (p > head.p) enqueue(Factor(p, 1) :: suffix)
      }
      /** rule 3 */
      else if (head.e == 1) {
        val p = largestPrime(head.p - 1)
        if (p > factors.tail.head.p) enqueue(Factor(p, 1) :: factors.tail)
      }
      /** rule 4 */
      if (factors.size == 1 && head.e == 2 && factors.head.p > 2) {
        val p = largestPrime(factors.head.p - 1)
        enqueue(Factor(p, 2) :: Nil)
      }
    }

    def enqueue(candidate: List[Factor]) {
      val n = nOf(candidate)
      if (n < limit) queue.enqueue(Entry(nPhi(candidate), n, candidate))
    }

    solve
  }

  val sId = if (args.size > 0) args(0).toInt else 1
  val limit = if(args.size > 1) args(1).toInt else 1e7.toInt
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}