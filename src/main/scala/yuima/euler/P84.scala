package yuima.euler

import scala.collection.mutable
import scala.util.Random

/** Monopoly odds
  * Problem 84
  * In the game, Monopoly, the standard board is set up in the following way:
  * GO A1 CC1 A2 T1 R1 B1 CH1 B2 B3 JAIL
  * H2   C1
  * T2   U1
  * H1   C2
  * CH3   C3
  * R4   R2
  * G3   D1
  * CC3   CC2
  * G2   D2
  * G1   D3
  * G2J F3 U2 F2 F1 R3 E3 E2 CH2 E1 FP
  *
  * A player starts on the GO square and adds the scoreson two 6-sided diceto determine the number of squares they advance in a clockwise direction.
  * Without any further rules we would expect to visit each square with equal probability: 2.5%.
  * However, landing on G2J (Go To Jail), CC (community chest), and CH (chance) changes this distribution.
  *
  * In addition to G2J, and one card from each of CC and CH, that orders the player to go directly to jail,
  * if a player rolls three consecutive doubles, they do not advance the result of their 3rd roll.
  * Instead they proceed directly to jail.
  *
  * At the beginning of the game, the CC and CH cards are shuffled.
  * When a player lands on CC or CH they take a card from the top of the respective pile and,
  * after following the instructions, it is returned to the bottom of the pile.
  * There are sixteen cards in each pile, but for the purpose of this problem we are only concerned with cards that order a movement;
  * any instruction not concerned with movement will be ignored and the player will remain on the CC/CH square.
  *
  * Community Chest (2/16 cards):
Advance to GO
Go to JAIL
Chance (10/16 cards):
Advance to GO
Go to JAIL
Go to C1
Go to E3
Go to H2
Go to R1
Go to next R (railway company)
Go to next R
Go to next U (utility company)
Go back 3 squares.
The heart of this problem concerns the likelihood of visiting a particular square.
That is, the probability of finishing at that square after a roll.
For this reason it should be clear that, with the exception of G2J for which the probability of finishing on it is zero,
the CH squares will have the lowest probabilities, as 5/8 request a movement to another square,
and it is the final square that the player finishes at on each roll that we are interested in.
We shall make no distinction between "Just Visiting" and being sent to JAIL,
and we shall also ignore the rule about requiring a double to "get out of jail",
assuming that they pay to get out on their next turn.

By starting at GO and numbering the squares sequentially from 00 to 39
we can concatenate these two-digit numbers to produce strings that correspond with sets of squares.

Statistically it can be shown that the three most popular squares, in order,
are JAIL (6.24%) = Square 10, E3 (3.18%) = Square 24, and GO (3.09%) = Square 00.
So these three most popular squares can be listed with the six-digit modal string: 102400.

If, instead of using two 6-sided dice, two 4-sided dice are used, find the six-digit modal string.
  */
object P84 extends App {

  def solution0 = {
    val total = 400000
    val counts = mutable.Map[String, Int]().withDefaultValue(0)

    val communityChestCards = Array("GO", "JAIL", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O", "O")
    val communityChestDeck = Iterator.continually(Random.shuffle(communityChestCards.toIterator)).flatten

    val chanceCards = Array("GO", "JAIL", "C1", "E3", "H2", "R1", "R", "R", "U", "BACK3", "O", "O", "O", "O", "O", "O")
    val chanceDeck = Iterator.continually(Random.shuffle(chanceCards.toIterator)).flatten

    val dice = (sides: Int) => (num: Int) => (1 to num).map(_ => Random.nextInt(sides) + 1)

    val squares = Array("GO", "A1", "CC1", "A2", "T1", "R1", "B1", "CH1", "B2", "B3", "JAIL", "C1", "U1", "C2", "C3", "R2", "D1", "CC2", "D2", "D3", "FP", "E1", "CH2", "E2", "E3", "R3", "F1", "F2", "U2", "F3", "G2J", "G1", "G2", "CC3", "G3", "R4", "CH3", "H1", "T2", "H2")
    val way = Iterator.continually(squares).flatten


    def move(faces: Int)(dices: Int)(repdigitCount: Int): (String, Int) = {
      val result = dice(faces)(dices)
      val newRepdigitCount = if (result.groupBy(identity).size == 1) repdigitCount + 1 else 0
      if (newRepdigitCount > 2) (way.dropWhile(_ != "JAIL").next(), 0)
      else (way.drop(result.sum - 1).next(), newRepdigitCount)
    }


    def goto(cell: String) = way.dropWhile(!_.startsWith(cell)).next()

    def nextRailway = goto("R")
    def nextUtility = goto("U")

    def back3 = way.drop(36).next()

    //    println(communityChestDeck.take(16).toList)
    //    println(communityChestDeck.take(16).toList)
    //    println(chanceDeck.take(16).toList)
    //    println(chanceDeck.take(16).toList)

    println(way.next)
    (0 /: (1 to total)) { (repdigitCount, i) =>
      val (next, rCount) = move(4)(2)(repdigitCount)
      val moved = next match {
        case _ if next.startsWith("CC") => communityChestDeck.next match {
          case "O" => next
          case dest => goto(dest)
        }
        case _ if next.startsWith("CH") => chanceDeck.next match {
          case "O" => next
          case "R" => nextRailway
          case "U" => nextUtility
          case "BACK3" => back3
          case dest => goto(dest)
        }
        case "G2J" => goto("JAIL")
        case _ => next
      }
      counts(moved) += 1
      //      println(next, moved)
      rCount
    }

    counts.map { pair =>
      (pair._1, squares.indexOf(pair._1), (pair._2.toDouble * 10000 / total).round / 100.0)
    }.toList.sortBy(-_._3).take(3)
  }

  solution0.foreach(println)
}

