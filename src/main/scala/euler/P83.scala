package euler

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

/** Path sum: four ways
  * Problem 83
  * NOTE: This problem is a significantly more challenging version of Problem 81.
  *
  * In the 5 by 5 costs below, the minimal path sum from the top left to the bottom right,
  * by moving left, right, up, and down, is indicated in bold red and is equal to 2297.
  *
  * ⎛⎝⎜⎜⎜⎜⎜⎜131201630537805673968036997322343427464975241039654221213718150111956331⎞⎠⎟⎟⎟⎟⎟⎟
  *
  * Find the minimal path sum, in costs.txt (right click and "Save Link/Target As..."),
  * a 31K text file containing a 80 by 80 costs, from the top left to the bottom right by moving left, right, up, and down.
  */
object P83 extends App {
  val file = getClass.getClassLoader.getResource("p083_matrix.txt").getPath
  val costs = Source.fromFile(file).getLines().map(_.split(',').map(_.toInt)).toArray

  //  val costs = Array(
  //    Array(131, 673, 234, 103, 18),
  //    Array(201, 96, 342, 965, 150),
  //    Array(630, 803, 746, 422, 111),
  //    Array(537, 699, 497, 121, 956),
  //    Array(805, 732, 524, 37, 331)
  //  )

  val result = new Dijkstracosts(costs).search(Pos(0, 0), Pos(costs.size - 1, costs.size - 1))

  println(result)

  case class Pos(i: Int, j: Int)

  class Dijkstracosts(val costs: Array[Array[Int]]) {
    val max = costs.map(_.max).max * costs.length * 2

    val distances = Array.fill(costs.length)(Array.fill(costs.length)(max))
    val scanned = Array.fill(costs.length)(Array.fill(costs.length)(false))
    val queue = mutable.PriorityQueue[(Pos, Int)]()(Ordering.by(node => -node._2))

    def distance(pos: Pos) = distances(pos.i)(pos.j)

    def cost(pos: Pos) = costs(pos.i)(pos.j)

    def isScanned(pos: Pos) = scanned(pos.i)(pos.j)

    def search(from: Pos, to: Pos) = {
      queue.clear()
      distances(from.i)(from.j) = 0
      queue.enqueue((from, distance(from)))
      distances(from.i)(from.j) = cost(from)
      doSearch(from: Pos, to: Pos)
    }

    @tailrec
    private def doSearch(from: Pos, to: Pos): Int = {
      if (queue.isEmpty) distance(to)
      else {
        val (node, _) = queue.dequeue()
        if (!isScanned(node)) {
          for {
            (i, j) <- Seq((node.i - 1, node.j), (node.i + 1, node.j), (node.i, node.j - 1), (node.i, node.j + 1))
            if i >= 0 && j >= 0 && i < costs.length && j < costs.length
          } {
            val dist = distance(node) + costs(i)(j)
            if (dist < distances(i)(j)) {
              distances(i)(j) = dist
              queue.enqueue((Pos(i, j), dist))
            }
          }
          scanned(node.i)(node.j) = true
        }
        doSearch(from, to)
      }
    }

    sealed class State
    case class Finished(cost: Int) extends State
    case class NotYet(cost: Int) extends State
  }
}