package yuima.euler

/** Almost equilateral triangles
  * Problem 94
  * It is easily proved that no equilateral triangle exists with integral length sides and integral area.
  * However, the almost equilateral triangle 5-5-6 has an area of 12 square units.
  *
  * We shall define an almost equilateral triangle to be a triangle for which two sides are equal and the third differs by no more than one unit.
  *
  * Find the sum of the perimeters of all almost equilateral triangles with integral side lengths and area and whose perimeters do not exceed one billion (1,000,000,000).
  */
object P94 extends App {
  val max = if(args.length > 0) args(0).toInt else 1000000000

  println(solution)

  /** for an almost equilateral triangle a-a-b,
    * b - a = 1 or -1.
    * If we consider two same right-angle triangles made by dividing b to a half,
    * (b/2)^2 + c^2 = a^2 and b * c / 2 should be an integer.
    *
    * Then,
    * (b/2)^2 + c^2 = (b +- 1)^2
    * b2/4 + c2 = b2 +-2b + 1
    * b2*(3/4) +- 2b + 1 - c2 = 0
    * (9/4)*b2 +-6b -3c2 + 3 = 0
    * ((3/2)b +- 2)2 - 3c2 = 1
    *
    * If b is odd, a is even and c should be even because of the area should be an integer.
    * But this leads to a contradiction in terms of a Pythagorean theorem: b^2 + (2*c)^2 = (2*a)^2
    * So b should be an even.
    * Then b/2 is an integer. So, c should be an integer.
    *
    * let x = ((3/2)b +- 2) and y = c. Then, the above equation is a Pell's equation and solutions can be
    * iteratively found using a recurrence relation.
    * the minimal solution of (x,y) is x = 2, y = 1
    * */
  def solution = {
    /** note: minimum (x, y) for n = 3 is x = 2, y = 1, but this cannot compose a triangle. */
    pellSolutions(3, PellSolution(2, 1)).drop(1).map { xy =>
      val (a, b) = pell2AlmostEquilateralTriangle(xy)
      perimeter(a, b)
    }.takeWhile(_ < max).sum
  }

  def pellSolutions(n: Int, s0: PellSolution) = {
    def next(s: PellSolution) = PellSolution(s0.x * s.x + n * s0.y * s.y, s0.x * s.y + s0.y * s.x)

    Iterator.iterate(s0)(next)
  }

  def pell2AlmostEquilateralTriangle(s: PellSolution) = {
    val remainder = s.x % 3
    val b = if (remainder == 1) (s.x + 2) / 3 * 2 else (s.x - 2) / 3 * 2
    val a = if (remainder == 1) b - 1 else b + 1
    (a, b)
  }

  def perimeter(a: Int, b: Int) = 2 * a + b

  case class PellSolution(x: Int, y: Int)

}
