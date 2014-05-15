package euler.scala

import scala.io.Source
import java.util.ArrayList

object P59 extends App {
  /**
   * XOR decryption
   * Problem 59
   * Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange).
   * For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.
   *
   * A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key.
   * The advantage with the XOR function is that using the same encryption key on the cipher text,
   * restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.
   *
   * For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes.
   * The user would keep the encrypted message and the encryption key in different locations, and without both "halves", it is impossible to decrypt the message.
   *
   * Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key.
   * If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message.
   * The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.
   *
   * Your task has been made easy, as the encryption key consists of three lower case characters.
   * Using cipher1.txt (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes,
   * and the knowledge that the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the original text.
   */

  /**
   * 文字１グラム頻度順のアスキーコード
   * ' ' = 32
   * 'e' = 101, 'E' = 69
   * 'a' = 97, 'A' = 65
   * 't' = 116, 'T' = 84
   * 'i' = 105, 'I' = 73
   * 文字２グラム頻度順
   * "th"
   * "st"
   * "ed"
   */
  val source = Source.fromFile(getClass.getClassLoader.getResource("cipher1.txt").getPath)
    .getLines.mkString("\n").split(",").map(_.toLowerCase().toInt)

  def getDistributions(cycle: Int = 3) = {
    val distributions = Array.fill(cycle)(new Array[Int](source.length / cycle + 1))
    for {
      i <- 0 to source.length / cycle - 1
      j <- 0 to cycle - 1
    } if (i * cycle + j < source.length) distributions(j)(i) = source(i * cycle + j)

    distributions.map(d =>
      (Map[Int, Int]() /: d) { (map, c) => if (c == 0) map else map + (c -> (map.getOrElse(c, 0) + 1)) }
        .toList.sortBy(_._2 * -1)
    )
  }

  def xors(n: Int) = ('a'.toInt to 'z'.toInt).map(key => (key, n ^ key)).toSeq

  def detectKey(distribution: List[(Int, Int)]) = {
    val space = xors(' '.toInt)
    val e = xors('e'.toInt)
    space.find(p => p._2 == distribution.head._1) match {
      case Some(p) => {
        e.find(q => q._2 == distribution.tail.head._1) match {
          case Some(q) if p._1 == q._1 => q._1
          case _ => {
            e.find(r => r._2 == distribution.tail.tail.head._1) match {
              case Some(r) if p._1 == r._1 => r._1
              case _ => 0
            }
          }
        }
      }
      case None => 0
    }
  }

  def decrypt(text: Array[Int], key: Array[Int]) = {
    var k = -1
    text.map(c => { k = (k + 1) % 3; (c ^ key(k)) })
  }

  val key = getDistributions().map(detectKey)
  val text = decrypt(source, key)
  val answer = text.sum
  //    println(key.map(_.toChar.toString).toList)
  //    println(text.map(_.toChar.toString).mkString)
  println(answer)

  //  getDistributions().foreach(d => println(d.take(5)))
  //  println(xors(' '.toInt).toString)
  //  println(xors('e'.toInt).toString)
  //  println(xors('a'.toInt).toString)
  //  println(xors('t'.toInt).toString)
  //  println(xors('i'.toInt).toString)
  //  println(xors('o'.toInt).toString)
  //  println(xors('h'.toInt).toString)
}