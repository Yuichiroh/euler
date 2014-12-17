package yuima.euler

object P52 extends App {
  /**
   * Permuted multiples
   * Problem 52
   * It can be seen that the number, 125874, and its double, 251748, contain exactly the same digits, but in a different order.
   *
   * Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x, and 6x, contain the same digits.
   */

  /** ６倍しても繰り上がりがあってはいけない。 すなわち、最上位の位は1。上から2番目の位は0から6。*/

  def isValidNum(n: Int) = {
    val list = n.toString
    list(0).toString.toInt == 1 && list(1).toString.toInt < 7
  }

  def composeOfSameDigits(n: Int, m: Int) = n.toString.map(_.toString.toInt).sorted == m.toString.map(_.toString.toInt).sorted

  def solution0 = Iterator.from(10).find(n => isValidNum(n) && (2 to 6).map(n*).forall(composeOfSameDigits(n, _))).get

  println(solution0)
}