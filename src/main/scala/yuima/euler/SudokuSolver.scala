/** Scala implementation of "A program to solve Sudoku."
  * http://www.cs.tufts.edu/~nr/cs257/archive/richard-bird/sudoku.pdf
  *
  * @author Yuichiroh Matsubayashi
  *         Created on 14/11/29.
  */
package yuima.euler

import scala.annotation.tailrec

class SudokuSolver(val boardSize: Int = 9,
                   val boxSize: Int = 3,
                   val cellValues: List[Char] = "123456789".toList,
                   val blank: Char => Boolean = (_: Char) == '.'
                    ) {

  type Matrix[T] = List[List[T]]
  type Board = Matrix[Char]
  type Choices[T] = List[T]
  type Row[T] = List[T]

  /** returns a list of solutions computed from a board with some empty cells */
  def solutions(b: Board) = search(prune(choices(b))).map(_.map(_.map(_.head)))

  def solution(b: Board) = search(prune(choices(b))).head.map(_.map(_.head))


  def isCorrect(b: Board) =
    (rows(b) forall noDuplication) && (cols(b) forall noDuplication) && (boxes(b) forall noDuplication)

  /** returns a list of small boxes composing the board. */
  def boxes[T](m: Matrix[T]) = {
    def group[T](list: List[T]) = list.grouped(boxSize).toList
    def ungroup[T](group: List[List[T]]) = group.flatten

    ungroup(group(m.map(group)).map(cols)).map(ungroup)
  }

  /** replaces blank entries in a board with all possible values in this puzzle, which produces [[Matrix]][List[Char]] */
  def choices(b: Board) = {
    def choose(cell: Char) = if (blank(cell)) cellValues else List(cell)

    b.map(_.map(choose))
  }

  /** generates a list of all possible boards from a given matrix of choices. */
  def possibleBoards[A](m: Matrix[Choices[A]]) = {
    def cartesianProduct[B](xs: List[B], yss: List[List[B]]) = for (x <- xs; ys <- yss) yield x :: ys

    val rowChoices = m.map(row => (row :\ Choices[Row[A]]()) { (cellChoices, rowChoices) => cartesianProduct(cellChoices, rowChoices) })
    (rowChoices :\ Choices[Matrix[A]]()) { (choices, matrices) => cartesianProduct(choices, matrices) }

    //    /** original code */
    //    def cartesianProduct[B](xss: List[List[B]]): List[List[B]] = xss match {
    //      case Nil => List(Nil)
    //      case xs :: rest => for (x <- xs; ys <- cartesianProduct(rest)) yield x :: ys
    //    }
    //    cartesianProduct(m.map(cartesianProduct))
  }

  /** explores solutions by incrementally fixing the cell which has the smallest number of choices.
    * Searching is stoped if the leaf board has one or more cells that may contain zero choices or
    * if the same fixed choice may occur in two or more positions in the same row, column or box.
    * */
  def search[T](m: Matrix[Choices[T]]): Choices[Matrix[Choices[T]]] = {
    def blocked(m: Matrix[Choices[T]]) = void(m) || !safe(m)

    def void(m: Matrix[Choices[T]]) = m.exists(_.contains(Nil))

    def safe(m: Matrix[Choices[T]]) =
      rows(m).forall(r => noDuplication(fixedEntries(r))) &&
        cols(m).forall(r => noDuplication(fixedEntries(r))) &&
        boxes(m).forall(r => noDuplication(fixedEntries(r)))

    if (blocked(m)) Nil
    else if (m.forall(_.forall(_.size == 1))) List(m)
    else expand(m).flatMap(x => search(prune(x)))
  }

  def rows[T](a: Matrix[T]) = a

  def cols[T](a: Matrix[T]) = a.transpose

  /** generate a list of matrices where the first cell having the smallest number of choices is expanded. */
  def expand[T](m: Matrix[Choices[T]]): Choices[Matrix[Choices[T]]] = {
    val minChoice = m.flatMap(_.map(_.size).filter(_ > 1)).min
    lazy val (rows1, row :: rows2) = m.splitAt(m.indexWhere(_.exists(best)))
    lazy val (row1, cs :: row2) = row.splitAt(row.indexWhere(best))

    def best(choices: Choices[T]) = choices.size == minChoice

    cs.map(c => rows1 ++ List(row1 ++ (Choices(c) :: row2)) ++ rows2)
  }

  /** removes already fixed entries from the unfixed choices of each row, column or box. */
  def prune[T](m: Matrix[Choices[T]]) =
    (pruneBy[T](boxes) andThen pruneBy(rows) andThen pruneBy(cols))(m)

  /** removes already fixed entries from the unfixed choices of each row, column or box. */
  def pruneBy[T](f: Matrix[Choices[T]] => Matrix[Choices[T]]) = (f andThen (_ map reduce)) andThen f

  def reduce[T](listOfChoices: List[Choices[T]]) = {
    def remove[T](fixedEntries: List[T])(choices: Choices[T]) =
      if (choices.size == 1) choices else choices diff fixedEntries

    val fes = fixedEntries(listOfChoices)
    listOfChoices.map(cs => remove(fes)(cs))
  }

  /** returns the fixed entries in a given row, column or box. */
  def fixedEntries[T](listOfChoices: List[Choices[T]]) = listOfChoices.filter(_.size == 1).flatten

  /** returns a human-readable string expression of the board. */
  def showBoard[T](b: Board) =
    b.map(row => row.grouped(boxSize).map(_.mkString(" ")).mkString(" | "))
      .grouped(boxSize).map(_.mkString("\n"))
      .mkString(s"\n${ (1 to boardSize / boxSize).map(_ => (1 to boxSize).map(_ => "-").mkString("-")).mkString("-+-") }\n")

  @tailrec
  private def noDuplication[T](line: List[T]): Boolean = line match {
    case Nil => true
    case n :: ns => !ns.contains(n) && noDuplication(ns)
  }

  object Choices {
    def apply[T](elms: T*): Choices[T] = elms.toList
  }

}
