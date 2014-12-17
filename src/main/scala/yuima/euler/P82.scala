package yuima.euler

import scala.io.Source

/** Path sum: three ways
  * Problem 82
  * NOTE: This problem is a more challenging version of Problem 81.
  *
  * The minimal path sum in the 5 by 5 matrix below,
  * by starting in any cell in the left column and finishing in any cell in the right column,
  * and only moving up, down, and right, is indicated in red and bold; the sum is equal to 994.
  *
  * ⎛⎝⎜⎜⎜⎜⎜⎜131201630537805673968036997322343427464975241039654221213718150111956331⎞⎠⎟⎟⎟⎟⎟⎟
  *
  * Find the minimal path sum, in matrix.txt (right click and "Save Link/Target As..."),
  * a 31K text file containing a 80 by 80 matrix, from the left column to the right column.
  */
object P82 extends App {
  val file = getClass().getClassLoader().getResource("p082_matrix.txt").getPath()
  val matrix = Source.fromFile(file).getLines().map(_.split(',').map(_.toInt)).toArray

  //  val matrix = Array(
  //    Array(131, 673, 234, 103, 18),
  //    Array(201, 96, 342, 965, 150),
  //    Array(630, 803, 746, 422, 111),
  //    Array(537, 699, 497, 121, 956),
  //    Array(805, 732, 524, 37, 331)
  //  )

  val chart = Array.fill(matrix.size)(Array.fill(matrix.size)(-1))

  def search {
    for (i <- 0 until matrix.size) chart(i)(0) = matrix(i)(0)
    for (j <- 1 until matrix.size) {
      for (i <- 0 until matrix.size)
        chart(i)(j) = chart(i)(j - 1) + matrix(i)(j)
      for (i <- 1 until matrix.size)
        chart(i)(j) = (chart(i - 1)(j) + matrix(i)(j)) min chart(i)(j)
      for (i <- matrix.size - 1 until 0 by -1)
        chart(i - 1)(j) = (chart(i)(j) + matrix(i - 1)(j)) min chart(i - 1)(j)
    }
  }
  search
  println(chart.map(_(matrix.size - 1)).min)
}