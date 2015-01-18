package yuima.euler

import scala.io.Source

/** Largest exponential
  * Problem 99
  *
  * Comparing two numbers written in index form like 211 and 37 is not difficult, as any calculator would confirm that 211 = 2048 < 37 = 2187.
  * However, confirming that 632382518061 > 519432525806 would be much more difficult, as both numbers contain over three million digits.
  *
  * Using base_exp.txt (right click and 'Save Link/Target As...'),
  * a 22K text file containing one thousand lines with a base/exponent pair on each line,
  * determine which line number has the greatest numerical value.
  *
  * NOTE: The first two lines in the file represent the numbers in the example given above.
  */
object P99 extends App {
  val file = getClass.getClassLoader.getResource("p099_base_exp.txt").getPath
  val pairs = Source.fromFile(file).getLines().map(_.split(",")).map(be => BE(be(0).toInt, be(1).toLong))

  def solution = pairs.zipWithIndex.maxBy(bei => bei._1.log)._2 + 1

  case class BE(base: Int, exp: Long) {
    val log = math.log(base) * exp
  }

  println(solution)
}
