package euler.scala

import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object P54 extends App {
  /**
   * Poker hands
   * Problem 54
   * In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:
   *
   * High Card: Highest value card.
   * One Pair: Two cards of the same value.
   * Two Pairs: Two different pairs.
   * Three of a Kind: Three cards of the same value.
   * Straight: All cards are consecutive values.
   * Flush: All cards of the same suit.
   * Full House: Three of a kind and a pair.
   * Four of a Kind: Four cards of the same value.
   * Straight Flush: All cards are consecutive values of same suit.
   * Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
   *
   * The cards are valued in the order:
   * 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
   *
   * If two players have the same ranked hands then the rank made up of the highest value wins;
   * for example, a pair of eights beats a pair of fives (see example 1 below).
   * But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below);
   * if the highest cards tie then the next highest cards are compared, and so on.
   *
   * Consider the following five hands dealt to two players:
   *
   * Hand	 	Player 1	 	Player 2	 	Winner
   * 1
   * 5H 5C 6S 7S KD
   * Pair of Fives
   * 2C 3S 8S 8D TD
   * Pair of Eights
   * Player 2
   *
   * 2
   * 5D 8C 9S JS AC
   * Highest card Ace
   * 2C 5C 7D 8S QH
   * Highest card Queen
   * Player 1
   *
   * 3
   * 2D 9C AS AH AC
   * Three Aces
   * 3D 6D 7D TD QD
   * Flush with Diamonds
   * Player 2
   *
   * 4
   * 4D 6S 9H QH QC
   * Pair of Queens
   * Highest card Nine
   * 3D 6D 7H QD QS
   * Pair of Queens
   * Highest card Seven
   * Player 1
   *
   * 5
   * 2H 2D 4C 4D 4S
   * Full House
   * With Three Fours
   * 3C 3D 3S 9S 9D
   * Full House
   * with Three Threes
   * Player 1
   *
   * The file, poker.txt, contains one-thousand random hands dealt to two players.
   * Each line of the file contains ten cards (separated by a single space):
   * the first five are Player 1's cards and the last five are Player 2's cards.
   * You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order,
   * and in each hand there is a clear winner.
   *
   * How many hands does Player 1 win?
   */

  val file = getClass().getClassLoader.getResource("poker.txt").getFile
  val hands = Source.fromFile(file).getLines.map(line => { val cards = line.split(" "); (cards.slice(0, 5), cards.slice(5, 10)) })

  def string2card(strs: Seq[String]) = {
    strs.map(str => {
      val fields = str.toCharArray
      val n = fields(0) match {
        case 'T' => 10
        case 'J' => 11
        case 'Q' => 12
        case 'K' => 13
        case 'A' => 14
        case m => m.toString.toInt
      }
      (n, fields(1))
    })
  }

  object Rank extends Enumeration {
    val HighCard, OnePair, TwoPairs, ThreeOfAKind, Straight, Flush, FullHouse, FourOfAKind, StraightFlush, RoyalFlush = Value
  }
  type Rank = Rank.Value

  def compareGroups(x: (Int, Int), y: (Int, Int)) = {
    if (x._2 == y._2) x._1 > y._1
    else x._2 > y._2
  }

  /** マークは価値計算に使わない。問題文によると、引き分けはないとの仮定を使って良いとのこと */
  def rankAndValue(hand: Seq[(Int, Char)]) = {
    val gathered = hand.groupBy(_._1).map(t => (t._1, t._2.size)).toList.sortWith((x, y) => compareGroups(x, y))
    gathered match {
      case List((a, 4), (b, 1)) => (Rank.FourOfAKind, List(a, b))
      case List((a, 3), (b, 2)) => (Rank.FullHouse, List(a, b))
      case List((a, 3), (b, 1), (c, 1)) => (Rank.ThreeOfAKind, List(a, b, c))
      case List((a, 2), (b, 2), (c, 1)) => (Rank.TwoPairs, List(a, b, c))
      case List((a, 2), (b, 1), (c, 1), (d, 1)) => (Rank.OnePair, List(a, b, c, d))
      case others => {
        if (others.head._1 - others.last._1 == 4) { // straight 循環なし
          if (hand.groupBy(_._2).size == 5) { // flush
            if (others.last._1 == 10 && others.head._1 == 14) (Rank.RoyalFlush, Nil) // royal
            else (Rank.StraightFlush, List(others.head._1))
          }
          else (Rank.Straight, List(others.head._1))
        }
        else {
          if (hand.groupBy(_._2).size == 5) (Rank.Flush, others.map(_._1))
          else (Rank.HighCard, others.map(_._1))
        }
      }
    }
  }

  def compareHands(hand1: (Rank, List[Int]), hand2: (Rank, List[Int])) = {
    if (hand1._1 == hand2._1) compareValue(hand1._2, hand2._2)
    else hand1._1 > hand2._1
  }

  def compareValue(values1: List[Int], values2: List[Int]): Boolean = {
    if (values1.head == values2.head) compareValue(values1.tail, values2.tail)
    else values1.head > values2.head
  }

  //  hands.take(20).map(t => (t._1.mkString(" "),t._2.mkString(" "),rank(string2card(t._1)), rank(string2card(t._2)), compareHands(rank(string2card(t._1)), rank(string2card(t._2))))).foreach(println)
  //  def solution0 = hands.take(20).map(t => compareHands(rank(string2card(t._1)), rank(string2card(t._2)))).filter(p=>p).size
  def solution0 = hands.map(t => compareHands(rankAndValue(string2card(t._1)), rankAndValue(string2card(t._2)))).filter(p => p).size
  println(solution0)
}