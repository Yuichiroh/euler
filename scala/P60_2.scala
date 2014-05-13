package euler.scala

import scala.collection.mutable
import scala.annotation.tailrec

object P60_2 extends App {
  /**
   * Prime pair sets
   * Problem 60
   * The primes 3, 7, 109, and 673, are quite remarkable.
   * By taking any two primes and concatenating them in any order the result will always be prime.
   * For example, taking 7 and 109, both 7109 and 1097 are prime.
   * The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
   *
   * Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
   */

  implicit def seq2long(num: Seq[Int]) = (0L /: num) { (s, i) => s * 10 + i }
  implicit def int2seq(n: Int) = n.toString.toCharArray().map(_.toString.toInt).toSeq

  case class PrimeList(list: List[Int], sum: Int, mod3: Int)

  val memoSuitablePrimes = mutable.Map[(Int, Int), Boolean]()
  def suitablePrimes(ps: PrimeList, max: Int, depth: Int) = Stream.from(ps.list.head + 1)
    .takeWhile(i => primes(i) < (max - ps.sum) / (depth - 1))
    .withFilter(i => {
      (ps.mod3 == 0 || primes(i) % 3 == ps.mod3) &&
        ps.list.forall(j =>
          memoSuitablePrimes.getOrElseUpdate(
            (j, i),
            primes.isPrime(primes(j) ++ primes(i)) && primes.isPrime(primes(i) ++ primes(j))
          )
        )
    })
    .map(i => new PrimeList(i :: ps.list, ps.sum + primes(i), if (ps.mod3 != 0) ps.mod3 else primes(i) % 3))

  @tailrec
  def search(list: Seq[PrimeList], max: Int, depth: Int): Seq[PrimeList] = depth match {
    case 1 => list
    case _ => {
      val newList = list.flatMap(ps => suitablePrimes(ps, max, depth))
      search(newList, max, depth - 1)
    }
  }

  val sieveMax = 100000
  val primes = new Primes(sieveMax)

  @tailrec
  def solution0(max: Int): (Seq[Int], Int) = {
    println(max)
    primes.expandSieve(max)
    search(
      (1 until primes.length).filter(primes(_) < max / 5).map(i => new PrimeList(List(i), primes(i), primes(i) % 3)),
      max,
      5
    ) match {
        case Nil => solution0((Math.sqrt(2) * max).toInt)
        case result => result.map(ps => (ps.list.map(primes(_)), ps.sum)).minBy(_._2)
      }
  }
  println("finish enumerating primes", primes.size)

  val initialMax = if (args.size > 0) args(0).toInt else 1469
  println(solution0(initialMax))
}
