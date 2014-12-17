package yuima.euler

import scala.io.Source

/** Passcode derivation
  * Problem 79
  * A common security method used for online banking is to ask the user for three random characters from a passcode.
  * For example, if the passcode was 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected reply would be: 317.
  *
  * The text file, keylog.txt, contains fifty successful login attempts.
  *
  * Given that the three characters are always asked for in order,
  * analyse the file so as to determine the shortest possible secret passcode of unknown length.
  */
object P79 extends App {
  val file = getClass.getClassLoader.getResource("p079_keylog.txt").getPath()
  val keys = Source.fromFile(file).getLines().toArray.distinct
  val order = keys.map(i => i.sliding(2).toArray).transpose.map(_.distinct.sorted).flatten

  val result = (Seq(order.head) /: order.tail) { (numbers, edge) =>
    val newones = numbers.flatMap(append(_, edge)).distinct.sortBy(_.toLong)
    val min = newones.head.size
    newones.takeWhile(_.size <= min)
  }

  println(result.map(_.toLong))

  def append(e1: String, e2: String): Seq[String] = {
    def index(str: String, ch: String, from: Int = 0): Int = str.indexOf(ch, from) match {
      case -1 => from - 1
      case i  => index(str, ch, from + 1)
    }

    def gen(ch: String, from: Int, to: Int) = for { i <- from to to } yield {
      val (p, s) = e1.splitAt(i)
      p + ch + s
    }

    val (f, t) = e2.splitAt(1)
    val fid = e1.indexOf(f)
    val tid = index(e1, t)

    if (fid < 0) {
      if (tid < 0) {
        for {
          j <- 0 to e1.size
          i <- 0 to j
        } yield {
          val (pt, st) = e1.splitAt(j)
          val (pf, sf) = pt.splitAt(i)
          pf + f + sf + t + st
        }
      }
      else gen(f, 0, tid)
    }
    else if (tid < 0) gen(t, fid + 1, e1.size)
    else if (fid < tid) Seq(e1)
    else gen(f, 0, tid) ++ gen(t, fid + 1, e1.size)
  }
}
