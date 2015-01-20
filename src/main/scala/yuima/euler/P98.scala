package yuima.euler

import scala.io.Source

/** Anagramic squares
  * Problem 98
  *
  * By replacing each of the letters in the word CARE with 1, 2, 9, and 6 respectively, we form a square number: 1296 = 36^2.
  * What is remarkable is that, by using the same digital substitutions, the anagram, RACE, also forms a square number: 9216 = 96^2.
  * We shall call CARE (and RACE) a square anagram word pair and specify further that leading zeroes are not permitted,
  * neither may a different letter have the same digital value as another letter.
  *
  * Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words,
  * find all the square anagram word pairs (a palindromic word is NOT considered to be an anagram of itself).
  *
  * What is the largest square number formed by any member of such a pair?
  * NOTE: All anagrams formed must be contained in the given text file.
  */
object P98 extends App {
  val file = getClass.getClassLoader.getResource("p098_words.txt").getPath
  val words = Source.fromFile(file).getLines().next.split(",").map(_.replaceAll("\"", "")).distinct

  val anagrams = words.groupBy(_.sorted).filter(_._2.length > 1)

  val maxLength = anagrams.map(_._1.length).max

  val sqares = Stream.from(0).map(n => n.toLong * n).takeWhile(_ < math.pow(10, maxLength))

  val anagramicSquares = sqares.groupBy(_.toString.sorted).filter(_._2.length > 1)

  def solution = {
    def canBeAligned(wordPair: List[Char], squarePair: List[Char], alignment: Map[Char, Char] = Map(), used: List[Char] = Nil): Boolean = {
      wordPair match {
        case Nil => true
        case w :: ord =>
          alignment.get(w) match {
            case Some(d) =>
              if (d == squarePair.head) canBeAligned(ord, squarePair.tail, alignment, used)
              else false
            case None =>
              if (used.contains(squarePair.head)) false
              else canBeAligned(ord, squarePair.tail, alignment + (w -> squarePair.head), squarePair.head :: used)
          }
      }
    }

    val anagramWordPairs = anagrams.flatMap(a => a._2.combinations(2).map(_.mkString))
    val anagramicSquarePairs = anagramicSquares.flatMap(s => s._2.combinations(2).flatMap(_.permutations.map(_.mkString))).toList.groupBy(_.length)

    val result = for {
      wp <- anagramWordPairs if anagramicSquarePairs.isDefinedAt(wp.length)
      asp <- anagramicSquarePairs(wp.length)
      if canBeAligned(wp.toList, asp.toList)
    } yield {
      val (a, b) = asp.splitAt(asp.length / 2)
      a.toLong max b.toLong
    }

    result.max
  }

  println(solution)
}
