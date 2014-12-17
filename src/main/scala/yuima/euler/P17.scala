package yuima.euler

object P17 extends App {
  /**
   * If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
   * If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
   *
   * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
   */

  /**
   * one -> 3, two -> 3, three -> 5, four -> 4, five -> 4,
   */
  val zero2nineteen = Array(0, 3, 3, 5, 4, 4, 3, 5, 5, 4, 3, 6, 6, 8, 8, 7, 7, 9, 8, 8) //0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19
  val zero2ninety = Array(0, 0, 6, 6, 5, 5, 5, 7, 6, 6) //0,10,20,30,40,50,60,70,80,90
  def numLength(num: Int): Int = num match {
    case _ if (num < 20) => zero2nineteen(num)
    case _ if (num < 100) => zero2ninety(num / 10) + (if (num % 10 > 0) zero2nineteen(num % 10) else 0)
    case _ if (num < 1000) => zero2nineteen(num / 100) + 7 + (if (num % 100 > 0) 3 + numLength(num % 100) else 0) // hundred -> 7, and -> 3
    case _ => 11 // one thousand
  }
  
  println((1 to 1000).map(numLength).sum)
}