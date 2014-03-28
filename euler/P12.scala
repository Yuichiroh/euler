package nlp.scala.euler

object P12 extends App {
  def triangleNumber(num: Int): Int = if (num == 0) 0 else num + triangleNumber(num - 1)
  def triangleNumbers = Iterator.from(1).map(triangleNumber)

  def factorize(num: Long, prime: Long = 2L): List[Long] = num match {
    case 1 => Nil
    case _ if prime * prime > num => num :: Nil
    case _ if num % prime == 0 => prime :: factorize(num / prime, prime)
    case _ => factorize(num, prime + 1)
  }

  def numDividers(num: Long) = factorize(num).groupBy(identity).map(_._2.length + 1).product

  println(triangleNumbers.find(numDividers(_) >= 500).get)
}