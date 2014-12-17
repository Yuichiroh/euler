package yuima.euler

object P51 extends App {
  /**
   * Prime digit replacements
   * Problem 51
   * By replacing the 1st digit of the 2-digit number *3, it turns out that six of the nine possible values:
   * 13, 23, 43, 53, 73, and 83, are all prime.
   *
   * By replacing the 3rd and 4th digits of 56**3 with the same digit,
   * this 5-digit number is the first example having seven primes among the ten generated numbers,
   * yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993.
   * Consequently 56003, being the first member of this family, is the smallest prime with this property.
   *
   * Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits)
   * with the same digit, is part of an eight prime value family.
   */

  /** 下一桁は入れ替えられることはなく、1,3,7,9 */
  /** 置き換える位の個数は3nでなければならず、置き換えない位の和は3mであってはならない */

  def isPrime(n: Int) = Iterator.from(2).takeWhile(m => m * m <= n).forall(n % _ != 0)

  def findDigitCombination(n: Int) = {
    val heads = n / 10
    val last = n % 10
    val list = heads.toString.map(_.toString.toInt)
    for {
      k <- (0 to 2) if (list.filter(_ != k).sum + last) % 3 != 0
      indexList = list.zipWithIndex.collect { case d if d._1 == k => d._2 }
//      indexList = (0 until list.size).filter(list(_) == k)
      replacingSize <- (3 to indexList.size by 3)
      combination <- indexList.combinations(replacingSize)
    } yield {
      for (newVal <- (0 to 9) if newVal != 0 || combination(0) != 0) yield { // !combination.contains(0)
        val array = list.toArray
        combination.foreach(array(_) = newVal)
        (array.mkString + last).toInt
      }
    }
  }

  def isEightPrimeFamily(nums: Seq[Int]) = (0 /: nums) { (n, num) => if (isPrime(num)) n + 1 else n } > 7

  /** 探索に重複有り。重複が起こらないように生成したほうがよい。 */
  def solution0 = Iterator.from(1111, 2).withFilter(n => n % 5 != 0 && isPrime(n) && findDigitCombination(n).exists(isEightPrimeFamily)).next

  println(solution0)
}