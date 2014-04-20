package euler.scala

import scala.collection.mutable.PriorityQueue
import scala.annotation.tailrec

object P50 extends App {
  /**
   * Consecutive prime sum
   * Problem 50
   * The prime 41, can be written as the sum of six consecutive primes:
   *
   * 41 = 2 + 3 + 5 + 7 + 11 + 13
   * This is the longest sum of consecutive primes that adds to a prime below one-hundred.
   *
   * The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.
   *
   * Which prime, below one-million, can be written as the sum of the most consecutive primes?
   */

  val max = if (args.size > 0) args(0).toInt else 1000000

  val isPrime = {
    val primes = Array.fill(max)(true)
    for {
      prime <- Iterator.from(2).takeWhile(n => n * n < max - 1).filter(primes)
      multi <- (prime * 2) to (max - 1) by prime
    } primes(multi) = false
    primes
  }

  val primes = (2 to (max - 1)).filter(isPrime).toList

  def diff(e: List[Int]) = e.size

  /** 列の長さ、先頭のインデックス、和、のPriorityQqueue */
  def search(pq: PriorityQueue[(Int, Int, Int)]): Int = {
    val (size, fid, sum) = pq.dequeue
    if (sum < max && isPrime(sum.toInt)) sum
    else {
      pq.enqueue((size - 1, fid, primes.slice(fid, size + fid - 1).sum))
      if (primes.size - fid == size)
        pq.enqueue((size - 1, fid + 1, primes.slice(fid + 1, size + fid).sum))
      search(pq)
    }
  }

  def solution0 = {
    val pq = PriorityQueue.newBuilder[(Int, Int, Int)]
    pq.enqueue((primes.size, 0, primes.sum))
    search(pq)
  }

  /**
   * しゃくとり法： maxを超えない範囲の列だけ探索する
   *  forward: 最大値を超えない範囲でtailを伸ばす。伸ばしきったらbackwardに。headからtailまでの長さが見つけている最長列以下になったら修了。
   *  backword: 最大値を超えていたら、超えない範囲までtailを縮める。縮めたら、headから始まり和が素数となる最長列を探す。headを１つ進めてforwardに。headがずらせなくなったら修了。
   */
  @tailrec
  def measuring(head: Int, tail: Int, sum: Int, longest: Int, longestSum: Int, forward: Boolean = true): Int = {
    if (forward) {
      if (tail < primes.size - 1 && sum + primes(tail + 1) < max) measuring(head, tail + 1, sum + primes(tail + 1), longest, longestSum, true)
      else if (tail - head > longest) measuring(head, tail, sum, longest, longestSum, false)
      else longestSum
    }
    else {
      if (sum >= max) measuring(head, tail - 1, sum - primes(tail), longest, longestSum, false)
      else {
        val (newl, newls) = longestSeq(head, tail, sum, longest)
        if (head < tail) {
          if (newl > longest) measuring(head + 1, tail, sum - primes(head), newl, newls, true)
          else measuring(head + 1, tail, sum - primes(head), longest, longestSum, true)
        }
        else longestSum
      }
    }
  }

  @tailrec
  def longestSeq(head: Int, tail: Int, sum: Int, longest: Int): (Int, Int) = {
    val diff = tail - head
    if (diff <= longest) (0, 0)
    else if (isPrime(sum)) (diff, sum)
    else longestSeq(head, tail - 1, sum - primes(tail), longest)
  }

  def solution1 = measuring(0, 1, 5, 0, 0)

  val sId = if (args.size > 1) args(1).toInt else 1
  def solution = sId match {
    case 0 => solution0
    case 1 => solution1
  }
  println(solution)
}