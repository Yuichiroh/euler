package euler

import scala.collection.mutable.ArrayBuffer

/** Singular integer right triangles
  * Problem 75
  * It turns out that 12 cm is the smallest length of wire that can be bent to form an integer sided right angle triangle in exactly one way,
  * but there are many more examples.
  *
  * 12 cm: (3,4,5)
  * 24 cm: (6,8,10)
  * 30 cm: (5,12,13)
  * 36 cm: (9,12,15)
  * 40 cm: (8,15,17)
  * 48 cm: (12,16,20)
  *
  * In contrast, some lengths of wire, like 20 cm, cannot be bent to form an integer sided right angle triangle,
  * and other lengths allow more than one solution to be found;
  * for example, using 120 cm it is possible to form exactly three different integer sided right angle triangles.
  *
  * 120 cm: (30,40,50), (20,48,52), (24,45,51)
  *
  * Given that L is the length of the wire,
  * for how many values of L ≤ 1,500,000 can exactly one integer sided right angle triangle be formed?
  */
object P75 extends App {
  val limit = args(0).toInt // 1500000

  /** Generating a tree of all primitive pythagorean triples
    * see Wikipedia: http://www.wikiwand.com/en/Tree_of_primitive_Pythagorean_triples
    */
  def solution = {
    val sumCount = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)

    def children(a: Int, b: Int, c: Int) {
      parent(2 * a + b - c, -2 * a + 2 * b + 2 * c, -2 * a + b + 3 * c)
      parent(2 * a + b + c, 2 * a - 2 * b + 2 * c, 2 * a - b + 3 * c)
      parent(2 * a - b + c, 2 * a + 2 * b + 2 * c, 2 * a + b + 3 * c)
    }

    def parent(a: Int, b: Int, c: Int) {
      val sum = a + b + c
      if (sum <= limit) {
        children(a, b, c)
        sumCount(sum) += 1
        Iterator.from(2).takeWhile(k => k * sum <= limit).foreach { k => sumCount(k * sum) += 1 }
      }
    }

    parent(3, 4, 5)
    //    list.size
    sumCount.count(_._2 < 2)
  }

  println(solution)

}