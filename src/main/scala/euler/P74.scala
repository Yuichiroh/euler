package euler

object P74 extends App {
  def factorialSum(n:Int) = n.toString().toCharArray().map(_.toString.toInt!).sum

  println(factorialSum(999999)) // 2177280
  println(factorialSum(2177280)) // 50406
  //  (9!) * 6 = 2177280
}