package nlp.scala.euler

object P1 extends App {
  def multiples(nums: Seq[Int], num: Int) = {
    nums.filter(n => n % num == 0)
  }

  //  println(multiples(multiples((1 until 1000), 3), 5).sum)
  //  println((1 until 1000).filter(n => n % 3 == 0 || n % 5 == 0).sum)

  //--------------

  /** 等差数列の和 */
  def arithmeticSeries(num: Int, max: Int) = {
    val n = (max - 1) / num
    num * n * (n + 1) / 2
  }
  val max = 1000
  println(arithmeticSeries(3, max) + arithmeticSeries(5, max) - arithmeticSeries(15, max))
}