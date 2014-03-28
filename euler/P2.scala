package nlp.scala.euler

object P2 extends App {
  def fib(a: Int = 1, b: Int = 1): Stream[Int] = a #:: fib(b, a + b)
  
  println(fib().takeWhile(_ < 4000000).filter(_ % 2 == 0).sum)
}