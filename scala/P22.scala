package euler.scala

import scala.io.Source

import scala.Array.canBuildFrom
import scala.math.BigInt.int2bigInt

object P22 extends App {
  /**
   * Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names,
   * begin by sorting it into alphabetical order.
   * Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.
   *
   * For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
   * So, COLIN would obtain a score of 938 × 53 = 49714.
   *
   * What is the total of all the name scores in the file?
   */

  def solution0 = {
    val file = getClass().getResource("names.txt").getFile
    val names = Source.fromFile(file).getLines.next.split(",").map(_.tail.init).sorted
    names.map(n => BigInt(n.map(c => c.toInt - 'A' + 1).sum)).zipWithIndex.map(e => e._1 * e._2).sum
  }

  println(solution0)

}