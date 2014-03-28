package nlp.scala.euler

object P3 extends App {
  def factorize(num: Long, prime: Long = 2L): List[Long] = num match {
    case 1 => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0 => prime :: factorize(num / prime, prime)
    case _ => factorize(num, prime + 1)
  }
  
  println(factorize(600851475143L).reverse.head)
}