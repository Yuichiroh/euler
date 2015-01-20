package yuima.euler

/** Arithmetic expressions
  * Problem 93
  * By using each of the digits from the set, {1, 2, 3, 4}, exactly once,
  * and making use of the four arithmetic operations (+, −, *, /) and brackets/parentheses,
  * it is possible to form different positive integer targets.
  *
  * For example,
  *
  * 8 = (4 * (1 + 3)) / 2
  * 14 = 4 * (3 + 1 / 2)
  * 19 = 4 * (2 + 3) − 1
  * 36 = 3 * 4 * (2 + 1)
  *
  * Note that concatenations of the digits, like 12 + 34, are not allowed.
  *
  * Using the set, {1, 2, 3, 4}, it is possible to obtain thirty-one different target numbers of which 36 is the maximum,
  * and each of the numbers 1 to 28 can be obtained before encountering the first non-expressible number.
  *
  * Find the set of four distinct digits, a < b < c < d, for which the longest set of consecutive positive integers,
  * 1 to n, can be obtained, giving your answer as a string: abcd.
  * */
object P93 extends App {

  lazy val ops = Seq(Fraction.+, Fraction.-, Fraction.*, Fraction./)

  println(solution)

  def solution = (1 to 9).combinations(4).maxBy { digits =>
    val positiveIntegerTargets = digits.permutations.foldLeft(Seq[Int]()) { (seq, ds) =>
      val fractions = ds.map(digit => Seq(Fraction(digit)))
      val results = (1 until ds.size).flatMap { i => operation(fractions.take(i), fractions.drop(i)) }
      seq ++ results.filter(_.denominator == 1).map(_.numerator.toInt).filter(_ > 0)
    }.distinct.sorted

    consecutiveMax(positiveIntegerTargets)
  }.mkString

  def operation(lefts: Seq[Seq[Fraction]], rights: Seq[Seq[Fraction]]): Seq[Fraction] = {
    val ls = if (lefts.size == 1) lefts(0) else (1 until lefts.size).flatMap(i => operation(lefts.take(i), lefts.drop(i))).distinct
    val rs = if (rights.size == 1) rights(0) else (1 until rights.size).flatMap(i => operation(rights.take(i), rights.drop(i))).distinct
    for {
      l <- ls
      r <- rs
      op <- ops if r.numerator.toInt != 0 || op != Fraction./
    } yield op(l)(r)
  }

  def consecutiveMax(nums: Seq[Int]) = Iterator.from(0).dropWhile(i => i + 1 == nums(i)).next()
}
