package nlp.scala.euler

object P6 extends App {
  val sum = (1 to 100).sum
  val squareSum = (1 to 100).map(n => n * n).sum
  
  println(sum * sum -squareSum)
}