package euler

import scala.io.Source

/** Path sum: four ways
  * Problem 83
  * NOTE: This problem is a significantly more challenging version of Problem 81.
  *
  * In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
  * by moving left, right, up, and down, is indicated in bold red and is equal to 2297.
  *
  * ⎛⎝⎜⎜⎜⎜⎜⎜131201630537805673968036997322343427464975241039654221213718150111956331⎞⎠⎟⎟⎟⎟⎟⎟
  *
  * Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."),
  * a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by moving left, right, up, and down.
  */
object P83 {
  val file = getClass().getClassLoader().getResource("p083_matrix.txt").getPath()
  val matrix = Source.fromFile(file).getLines().map(_.split(',').map(_.toInt)).toArray
}