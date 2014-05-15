package euler.scala

import scala.io.Source

object P42 extends App {
  /**
   * Coded triangle numbers
   * Problem 42
   * The nth term of the sequence of triangle numbers is given by, tn = (1/2)n(n+1); so the first ten triangle numbers are:
   *
   * 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
   *
   * By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value.
   * For example, the word value for SKY is 19 + 11 + 25 = 55 = t10.
   * If the word value is a triangle number then we shall call the word a triangle word.
   *
   * Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words,
   * how many are triangle words?
   */

  val file = getClass().getClassLoader.getResource("words.txt").getFile

  /** A is 65 */
  val char2int = ('A' to 'Z').map(c => (c, c.toInt - 64)).toMap

  /** 0 = n^2 + n -2t */
  def isTriangleNum(n: Int) = Math.sqrt(1 + 8 * n).isValidInt

  def word2int(w: String) = w.toUpperCase.toCharArray.map(char2int).sum

  def solution0 = Source.fromFile(file).getLines.next.replaceAll("\"", "").split(",").map(word2int).count(isTriangleNum)

  println(solution0)
}