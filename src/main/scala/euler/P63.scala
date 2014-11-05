package euler

object P63 extends App {
  /**
   * Powerful digit counts
   * Problem 63
   * The 5-digit number, 16807=7^5, is also a fifth power. Similarly, the 9-digit number, 134217728=8^9, is a ninth power.
   *
   * How many n-digit positive integers exist which are also an nth power?
   */

  def suitablePowers(power: Int) = Stream.from(1).map(base => math.log10(math.pow(base, power))).dropWhile(_ < power - 1).takeWhile(_ < power).size

  def solution0 = Stream.from(1).map(suitablePowers).takeWhile(power => math.log10(math.pow(2, power)) < power).sum

  println(solution0)
}