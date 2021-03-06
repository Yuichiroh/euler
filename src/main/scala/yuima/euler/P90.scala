package yuima.euler

/** Cube digit pairs
  * Problem 90
  * Each of the six faces on a cube has a different digit (0 to 9) written on it;
  * the same is done to a second cube. By placing the two cubes side-by-side in different positions we can form a variety of 2-digit numbers.
  *
  * For example, the square number 64 could be formed:
  * 6 4
  *
  * In fact, by carefully choosing the digits on both cubes it is possible to display all of the square numbers below one-hundred:
  * 01, 04, 09, 16, 25, 36, 49, 64, and 81.
  *
  * For example, one way this can be achieved is by placing {0, 5, 6, 7, 8, 9} on one cube and {1, 2, 3, 4, 8, 9} on the other cube.
  *
  * However, for this problem we shall allow the 6 or 9 to be turned upside-down so that an arrangement like
  * {0, 5, 6, 7, 8, 9} and {1, 2, 3, 4, 6, 7} allows for all nine square numbers to be displayed;
  * otherwise it would be impossible to obtain 09.
  *
  * In determining a distinct arrangement we are interested in the digits on each cube, not the order.
  *
  * {1, 2, 3, 4, 5, 6} is equivalent to {3, 6, 4, 1, 2, 5}
  * {1, 2, 3, 4, 5, 6} is distinct from {1, 2, 3, 4, 5, 9}
  *
  * But because we are allowing 6 and 9 to be reversed,
  * the two distinct sets in the last example both represent the extended set {1, 2, 3, 4, 5, 6, 9} for the purpose of forming 2-digit numbers.
  *
  * How many distinct arrangements of the two cubes allow for all of the square numbers to be displayed?
  */
object P90 extends App {

  type Dice = IndexedSeq[Int]

  val max = if(args.length > 0) args(0).toInt else 100


  val squares = Stream.from(1).map(n => n * n).takeWhile(_ < max)

  lazy val diceSize = squares.max.toString.size
  lazy val squarefaceSets = squares.map(sq => "%%0%dd".format(diceSize).format(sq)).toList.map(_.toCharArray.map(_.toString.toInt))

  val possibleDices = (0 to 9).combinations(6).toSeq

  def diceCombinations(size: Int): Seq[IndexedSeq[Dice]] =
    if (size > 1) possibleDices.flatMap(dice => diceCombinations(size - 1).map(dices => dices :+ dice))
    else possibleDices.map(IndexedSeq(_))

  def isValidDices(dices: IndexedSeq[Dice]) = squarefaceSets.forall { set =>
    set.permutations.exists { faces =>
      (0 until faces.size).forall { i =>
        faces(i) match {
          case 6 | 9 => dices(i).contains(6) || dices(i).contains(9)
          case n => dices(i).contains(n)
        }
      }
    }
  }

  println(diceCombinations(diceSize).map(_.sortBy(_.mkString)).toSet.filter(isValidDices).size)
}
