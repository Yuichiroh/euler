package yuima.euler

import scala.collection.mutable

object P60 extends App {
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
  def isPrimeMillerRabin(n: Long) = {
    def divBy2(n: Long): Long = if (n % 2 != 0) n else divBy2(n / 2)

    def pow(base: Long, power: Long, mod: Long, result: Long): Long = {
      if (power > 0) pow((base * base) % mod, power >> 1, mod, if ((power & 1) == 1) (result * base) % mod else result)
      else result
    }

    val d = divBy2(n - 1)
    List(2, 3, 5, 7).forall(a => {
      var t = d
      var y = pow(a, t, n, 1L)
      while ((t != n - 1) && (y != 1) && (y != n - 1)) {
        y = (y * y) % n
        t <<= 1
      }
      y == n - 1 || (t & 1) != 0
    })
  }

  implicit def seq2int(num: Seq[Int]) = (0 /: num) { (s, i) => s * 10 + i }
  implicit def seq2long(num: Seq[Int]) = (0L /: num) { (s, i) => s * 10 + i }

  implicit def int2seq(n: Int) = n.toString.toCharArray().map(_.toString.toInt).toSeq

  /**
   * ある二つの素数を取ったときに、各位の和 mod 3 に 2と1が出てくると、そのペアは、必ず３で割り切れる。
   * したがって、nつ組には、各位の和 mod 3は、2と1のうちいづれか１つしか含まれない。
   * List[(List[Int], Int, Int) は、(nつ組, 各位の和 mod 3, nつ組の和)
   */
  val memo = mutable.Map[(Int, Int), Boolean]()
  def concatPairPrimes(lists: List[(List[Int], Int, Int)], max: Int, limit: Int, depth: Int, primes: Array[Int], isPrime: Array[Boolean],
    check: ((List[Int], Int, Int), Int, Int) => Boolean): List[(List[Int], Int, Int)] = {
    //    println(depth)
    if (depth == 1) lists
    else {
      val newLists = lists.flatMap(ps =>
        (0 until primes.size).drop(ps._1.head).filter(i => {
          check(ps, primes(i), max) && ps._1.forall(j => memo.getOrElseUpdate((i, j), {
            val p1p2 = primes(i) ++ primes(j)
            val p2p1 = primes(j) ++ primes(i)
            isPrime(p1p2) && isPrime(p2p1)
          }))
        }).map(i => (i :: ps._1, if (ps._2 != 0) ps._2 else primes(i) % 3, ps._3 + primes(i)))
      )
      concatPairPrimes(newLists, max, limit, depth - 1, primes, isPrime, check)
    }
  }

  def concatPairPrimes2(lists: List[(List[Int], Int, Int)], max: Int, limit: Int, depth: Int, primes: Array[Int], isPrime: Array[Boolean],
    check: ((List[Int], Int, Int), Int, Int) => Boolean): List[(List[Int], Int, Int)] = {
    println(depth)
    if (depth == 1) lists
    else {
      val newLists = lists.flatMap(ps =>
        (0 until primes.size).drop(ps._1.head).filter(i => {
          check(ps, primes(i), max) && ps._1.forall(j => memo.getOrElseUpdate((i, j), {
            val p1p2 = primes(i) ++ primes(j)
            val p2p1 = primes(j) ++ primes(i)
            (if (seq2long(p1p2) < limit) isPrime(p1p2) else isPrimeMillerRabin(p1p2)) &&
              (if (seq2long(p2p1) < limit) isPrime(p2p1) else isPrimeMillerRabin(p2p1))
          }))
        }).map(i => (i :: ps._1, if (ps._2 != 0) ps._2 else primes(i) % 3, ps._3 + primes(i)))
      )
      concatPairPrimes2(newLists, max, limit, depth - 1, primes, isPrime, check)
    }
  }

  def checkForFirstSearch(ps: (List[Int], Int, Int), p: Int, max: Int) = (ps._2 == 0 || p % 3 == ps._2)
  def checkForSmallestSearch(ps: (List[Int], Int, Int), p: Int, max: Int) = (ps._2 == 0 || p % 3 == ps._2) && (ps._3.toLong + p < max)

  def search(max: Int, depth: Int = 5, limit: Int,
    check: ((List[Int], Int, Int), Int, Int) => Boolean) = {
    //    println((max, limit))
    val isPrime = Array.fill(limit)(true)
    val primes = {
      for {
        prime <- Iterator.from(2).takeWhile(n => n * n < limit - 1).filter(isPrime)
        multi <- (prime * 2) to (limit - 1) by prime
      } isPrime(multi) = false
      (3 to max).filter(isPrime).toArray
    }
    println("finish enumerating primes", primes.size)
    concatPairPrimes(Iterator.from(0).take(primes.size).map(n => (List(n), primes(n).sum % 3, primes(n))).toList, max, limit, depth, primes, isPrime, check)
  }

  def search2(max: Int, depth: Int = 5, limit: Int,
    check: ((List[Int], Int, Int), Int, Int) => Boolean) = {
    println((max, limit))
    val isPrime = Array.fill(limit)(true)
    val primes = {
      for {
        prime <- Iterator.from(2).takeWhile(n => n * n < limit - 1).filter(isPrime)
        multi <- (prime * 2) to (limit - 1) by prime
      } isPrime(multi) = false
      (3 to max).filter(isPrime).toArray
    }
    println("finish enumerating primes", primes.size)
    concatPairPrimes2(Iterator.from(0).take(primes.size).map(n => (List(n), primes(n).sum % 3, primes(n))).toList, max, limit, depth, primes, isPrime, check)
  }

  def limitForFirstSerch(max: Int) = (max.toString + max.toString).toInt + 1
  def limitForSmallestSerch(max: Int) = {
    val concatTail = Math.pow(10, max.toString.size - 1).toInt
    ((max - concatTail).toString + concatTail.toString).toInt + 1
  }

  val depth = 5
  val max = if (args.size > 1) args(1).toInt else 673

  def solution0 = {
    var result = search(max, depth, limitForFirstSerch(max), checkForFirstSearch)
    var limit = max
    while (result == Nil) {
      limit = (limit * Math.sqrt(2)).toInt + 1
      result = search(limit, depth, limitForFirstSerch(limit), checkForFirstSearch)
    }
    //解候補aを見つけたら、a以下の素数でdepth=4のときに題意を満たす全ての４つ組(和をbとする)について、bからa-bの範囲の素数をみて、aより小さい解がないか確かめる必要がある。
    val temp = result.map(_._3).min
    if (temp < max) temp
    else search(temp, depth, limitForSmallestSerch(temp), checkForSmallestSearch) match {
      case Nil => temp
      case list => list.map(_._3).min
    }
  }

  def solution1 = {
    var result = search2(max, depth, max ++ max, checkForFirstSearch)
    var limit = max
    while (result == Nil) {
      limit = (limit * Math.sqrt(2)).toInt + 1
      result = search2(limit, depth, 100000, checkForFirstSearch)
    }
    val temp = result.map(_._3).min
    if (temp < max) temp
    else search2(temp, depth, 1000000, checkForSmallestSearch) match {
      case Nil => temp
      case list => list.map(_._3).min
    }
  }

  val sId = if (args.size > 0) args(0).toInt else 1

  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}
