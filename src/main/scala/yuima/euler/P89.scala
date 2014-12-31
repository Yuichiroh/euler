package yuima.euler

import scala.io.Source

/**
 * Roman numerals
 * Problem 89
 * For a number written in Roman numerals to be considered valid there are basic rules which must be followed.
 * Even though the rules allow some numbers to be expressed in more than one way there is always a "best" way of writing a particular number.
 *
 * For example, it would appear that there are at least six ways of writing the number sixteen:
 *
 * IIIIIIIIIIIIIIII
 * VIIIIIIIIIII
 * VVIIIIII
 * XIIIIII
 * VVVI
 * XVI
 *
 * However, according to the rules only XIIIIII and XVI are valid, and the last example is considered to be the most efficient,
 * as it uses the least number of numerals.
 *
 * The 11K text file, roman.txt (right click and 'Save Link/Target As...'), contains one thousand numbers written in valid,
 * but not necessarily minimal, Roman numerals; see About... Roman Numerals for the definitive rules for this problem.
 *
 * Find the number of characters saved by writing each of these in their minimal form.
 *
 * Note: You can assume that all the Roman numerals in the file contain no more than four consecutive identical units.
 */
object P89 extends App {
  val r2num = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

  val solution = Source.fromFile(args(0)).getLines().map { line =>
    line.size - num2rs(rs2num(line.toList)).size
  }.sum

  def rs2num(rs: List[Char]): Int = rs match {
    case Nil => 0
    case 'I' :: 'V' :: rest => 4 + rs2num(rest)
    case 'I' :: 'X' :: rest => 9 + rs2num(rest)
    case 'X' :: 'L' :: rest => 40 + rs2num(rest)
    case 'X' :: 'C' :: rest => 90 + rs2num(rest)
    case 'C' :: 'D' :: rest => 400 + rs2num(rest)
    case 'C' :: 'M' :: rest => 900 + rs2num(rest)
    case c :: rest => r2num(c) + rs2num(rest)
  }

  def num2rs(num: Int) = m(num)

  def m(num: Int) = "M" * (num / 1000) + cm(num % 1000)

  def cm(num: Int) = "CM" * (num / 900) + d(num % 900)

  def d(num: Int) = "D" * (num / 500) + cd(num % 500)

  def cd(num: Int) = "CD" * (num / 400) + c(num % 400)

  def c(num: Int) = "C" * (num / 100) + xc(num % 100)

  def xc(num: Int) = "XC" * (num / 90) + l(num % 90)

  def l(num: Int) = "L" * (num / 50) + xl(num % 50)

  def xl(num: Int) = "XL" * (num / 40) + x(num % 40)

  def x(num: Int) = "X" * (num / 10) + ix(num % 10)

  def ix(num: Int) = "IX" * (num / 9) + v(num % 9)

  def v(num: Int) = "V" * (num / 5) + iv(num % 5)

  def iv(num: Int) = "IV" * (num / 4) + i(num % 4)

  def i(num: Int) = "I" * num

  println(solution)
}
