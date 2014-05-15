package euler.scala

object P62 extends App {
  /**
   * Cubic permutations
   * Problem 62
   * The cube, 41063625 (345^3), can be permuted to produce two other cubes: 56623104 (384^3) and 66430125 (405^3).
   * In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
   *
   * Find the smallest cube for which exactly five permutations of its digits are cube.
   */

  val cubes = Iterator.from(5).map(Math.pow(_, 3).toLong)

  val cubesWithSameDigitSize = Iterator.from(3).map(n => cubes.takeWhile(_ < Math.pow(10, n).toLong))

  val largestSizeCubePermutations = cubesWithSameDigitSize.map(cubes =>
    cubes.toSeq.groupBy(cube => cube.toString.toCharArray().sorted.mkString.toLong).map(_._2)
  ).map(permutations => permutations.groupBy(_.size).maxBy(_._1))

  println(largestSizeCubePermutations.dropWhile(_._1 < 5).next._2.map(_.head).min)
}