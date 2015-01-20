package yuima.euler

/** Cuboid route
  * Problem 86
  * A spider, S, sits in one corner of a cuboid room, measuring 6 by 5 by 3, and a fly, F, sits in the opposite corner.
  * By travelling on the surfaces of the room the shortest "straight line" distance from S to F is 10 and the path is shown on the diagram.
  *
  * However, there are up to three "shortest" path candidates for any given cuboid and the shortest route doesn't always have integer length.
  * It can be shown that there are exactly 2060 distinct cuboids, ignoring rotations, with integer dimensions,
  * up to a maximum size of M by M by M, for which the shortest route has integer length when M = 100.
  * This is the least value of M for which the number of solutions first exceeds two thousand; the number of solutions when M = 99 is 1975.
  *
  * Find the least value of M such that the number of solutions first exceeds one million.
  */
object P86 extends App {
  val sId = if (args.size > 0) args(0).toInt else 1
  val thres = if(args.size > 1) args(1).toInt else 1000000

  def solution = sId match {
    case 0 => solution0(thres)
    case 1 => solution1(thres)
  }

  println(solution)

  /** [total 37756ms] */
  def solution0(thres: Int) = {
    import scala.annotation._

    @tailrec
    def result(max: Int, prev: Int = 0): Int = {
      val next = prev + (for {
        b <- 1 to max
        a <- 1 to b
      } yield hasIntegerShortestRoute(a, b, max)).count(identity)
      if (next > thres) max
      else result(max + 1, next)
    }

    result(2)
  }

  /** When considering cuboid where the side lengths are a, b, and c, there are three shortest route candidates:
    *
    * sqrt( (a + b)^2 + c^2)
    * sqrt( (a + c)^2 + b^2)
    * sqrt( (b + c)^2 + a^2).
    *
    * But if we choose rectangles s.t. a <= b <= c, then the shortest route is sqrt( (a + b)^2 + c^2)
    * because 2ab <= 2ac <= 2bc.
    * */
  def hasIntegerShortestRoute(a: Int, b: Int, c: Int) = {
    require(a <= b && b <= c)
    val shortest = math.pow(a + b, 2) + math.pow(c, 2)
    math.sqrt(shortest).isValidInt
  }

  /** generates Pythagorean triples first, then divides one of the sides as the resulting sides composes a cube satisfying the condition.
    * [total 2235ms]
    * */
  def solution1(thres: Int) = {
    def search(max: Int): Int = {
      val counts = for {
      //        m <- 1 to max
      //        n <- 1 until m
        m <- 2 until ((1 + math.sqrt(2)) * max).ceil.toInt
        n <- math.sqrt(math.pow(m, 2) - 2 * max).toInt max 1 to max / m
        if gcd(m, n) == 1
        //        m2 = math.pow(m, 2).toInt
        //        n2 = math.pow(n, 2).toInt
        a = math.pow(m, 2).toInt - math.pow(n, 2).toInt if a % 2 == 1
        b = 2 * m * n
      //        c = m2 + n2
      //        if a < c && b < c
      //        if a <= 2 * max && b <= 2 * max
      } yield {
        val p = a min b
        val q = a max b
        //        val ks = Iterator.from(1).takeWhile(k => k * p <= 2 * max && k * q <= 2 * max)
        val ks = Iterator.from(1).take((2 * max / p) min (2 * max / q))

        //        println(s"m=$m n=$n k=$ks", p, q, c)
        ks.map { k =>
          val countP = if (k * q <= max) k * p / 2 else 0
          val countQ = if (k * p <= max) 0 max (k * p - (k * q - 1) / 2) else 0
          countP + countQ
        }.sum
      }

      val count = counts.sum
      if (count > thres) max
      else search(max + 1)
    }
    search(1)
  }
}
