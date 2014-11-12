import scala.annotation.tailrec
import scala.collection.mutable

val costs = Array(
  Array(131, 673, 234, 103, 18),
  Array(201, 96, 342, 965, 150),
  Array(630, 803, 746, 422, 111),
  Array(537, 699, 497, 121, 956),
  Array(805, 732, 524, 37, 331)
)
val result = search(Pos(0, 0), Pos(costs.size - 1, costs.size - 1)
val max = costs.map(_.max).max
val distances = Array.fill(costs.length)(Array.fill(costs.length)(max))
val prev = Array.fill(costs.length)(Array.fill[Pos](costs.length)(null))
val queue = mutable.PriorityQueue[Pos]()(Ordering.by(pos => costs(pos.i)(pos.j)))

def distance(pos:Pos) = distances(pos.i)(pos.j)

def cost(pos:Pos) = costs(pos.i)(pos.j)

def search(from: Pos, to: Pos) = {
  queue.clear()

  for {
    i <- 0 until costs.length
    j <- 0 until costs.length
  }
    queue.enqueue(Pos(i, j))

  costs(from.i)(from.j) = 0
  doSearch(from: Pos, to: Pos)
}

@tailrec
private def doSearch(from: Pos, to: Pos): Int = {
  if (queue.isEmpty) costs(to.i)(to.j)
  else {
    val node = queue.dequeue()
    println(node, distance(node))
    for {
      i <- 0 until costs.length if (node.i - i).abs == 1
      j <- 0 until costs.length if (node.j - j).abs == 1
    } {
      val distance = distance(node) + costs(i)(j)
      if (distance < distances(i)(j)) distances(i)(j) = distance
    }
    doSearch(from, to)
  }
}
sealed class State
case class Finished(cost: Int) extends State
case class NotYet(cost: Int) extends State
println(result)
case class Pos(i: Int, j: Int)
//  println(max)
max

