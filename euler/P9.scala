package nlp.scala.euler

object P9 extends App {
  val sum = 1000
  val answers = for (a <- 1 to sum / 3; b <- a + 1 to sum / 2; c = sum - a - b if a * a + b * b == c * c) yield a*b*c
  answers.foreach(println)
}