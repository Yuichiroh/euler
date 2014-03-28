package nlp.scala.euler

object P5 extends App {
  def gcd(m: Int, n: Int): Int = if (n == 0) m else gcd(n, m % n)

  def lcm(m: Int, n: Int): Int = m * (n / gcd(m, n))
  
  println((11 to 20).reduceLeft(lcm))
}