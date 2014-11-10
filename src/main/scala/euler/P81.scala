package euler

import scala.io.Source

/** Path sum: two ways
  * Problem 81
  * In the 5 by 5 matrix below, the minimal path sum from the top left to the bottom right,
  * by only moving to the right and down, is indicated in bold red and is equal to 2427.
  *
  * ⎛⎝⎜⎜⎜⎜⎜⎜131201630537805673968036997322343427464975241039654221213718150111956331⎞⎠⎟⎟⎟⎟⎟⎟
  *
  * Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."),
  * a 31K text file containing a 80 by 80 matrix, from the top left to the bottom right by only moving right and down.
  */
object P81 extends App {
  val file = getClass().getClassLoader().getResource("p081_matrix.txt").getPath()
  val matrix = Source.fromFile(file).getLines().map(_.split(',').map(_.toInt)).toArray

  //  val matrix = Array(
  //    Array(131, 673, 234, 103, 18),
  //    Array(201, 96, 342, 965, 150),
  //    Array(630, 803, 746, 422, 111),
  //    Array(537, 699, 497, 121, 956),
  //    Array(805, 732, 524, 37, 331)
  //  )

  val chart = Array.fill(matrix.size)(Array.fill(matrix.size)(-1))

  def dpSearch {
    for {
      i <- 0 until matrix.size
      j <- 0 until matrix.size
    } {
      if (i == 0) {
        if (j == 0) chart(i)(j) = matrix(i)(j)
        else chart(i)(j) = chart(i)(j - 1) + matrix(i)(j)
      }
      else if (j == 0) chart(i)(j) = chart(i - 1)(j) + matrix(i)(j)
      else chart(i)(j) = (chart(i - 1)(j) min chart(i)(j - 1)) + matrix(i)(j)
    }
  }
  dpSearch
  println(chart(matrix.size - 1)(matrix.size - 1))
}