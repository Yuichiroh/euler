package nlp.scala.euler

object P4 extends App {
  def isPalindrome(num: String) = num.reverse == num

  // 以下のコードよりも、priority queuewを使う方法のほうが速い
  val palindromes = for (x <- 100 to 999; y <- x to 999; num = x * y if isPalindrome(num.toString)) yield num
  println(palindromes.max)
}