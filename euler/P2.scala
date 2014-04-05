package euler.scala

object P2 extends App {
  /**
   * Even Fibonacci numbers
   * Problem 2
   * Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:
   *
   * 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
   *
   * By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
   */

  def fib(a: Int = 1, b: Int = 2): Stream[Int] = a #:: fib(b, a + b)

  def solution0 = fib().takeWhile(_ < 4000000).filter(_ % 2 == 0).sum

  def solution1 = {
    lazy val fib2: Stream[Int] = 1 #:: fib2.scanLeft(2)(_ + _)
    fib2.takeWhile(_ < 4000000).filter(_ % 2 == 0).sum
  }

  val solutions = List(solution0, solution1)
  val sId = if (args.size > 0) args(0).toInt else 0
  println(solutions(sId))
}