package yuima.euler

/**
 * Product-sum numbers
 * Problem 88
 * A natural number, N, that can be written as the sum and product of a given set of at least two natural numbers,
 * {a1, a2, ... , ak} is called a product-sum number: N = a1 + a2 + ... + ak = a1 × a2 × ... × ak.
 *
 * For example, 6 = 1 + 2 + 3 = 1 × 2 × 3.
 *
 * For a given set of size, k, we shall call the smallest N with this property a minimal product-sum number.
 * The minimal product-sum numbers for sets of size, k = 2, 3, 4, 5, and 6 are as follows.
 *
 * k=2: 4 = 2 × 2 = 2 + 2
 * k=3: 6 = 1 × 2 × 3 = 1 + 2 + 3
 * k=4: 8 = 1 × 1 × 2 × 4 = 1 + 1 + 2 + 4
 * k=5: 8 = 1 × 1 × 2 × 2 × 2 = 1 + 1 + 2 + 2 + 2
 * k=6: 12 = 1 × 1 × 1 × 1 × 2 × 6 = 1 + 1 + 1 + 1 + 2 + 6
 *
 * Hence for 2≤k≤6, the sum of all the minimal product-sum numbers is 4+6+8+12 = 30; note that 8 is only counted once in the sum.
 *
 * In fact, as the complete set of minimal product-sum numbers for 2≤k≤12 is {4, 6, 8, 12, 15, 16}, the sum is 61.
 *
 * What is the sum of all the minimal product-sum numbers for 2≤k≤12000?
 */
object P88 extends App {
  val max = if(args.length > 0) args(0).toInt else 12000
  val minimals = Array.fill(max + 1)(0)

  /** product-sum ps に e をかける場合、e1 < e2 なら、k1 < k2 */
  def productSums(ps: ProductSum): Iterator[ProductSum] =
    Iterator(ps) ++ Iterator.from(ps.lastTerm).map(ps + _).takeWhile(_.k <= max).flatMap(productSums)

  /** 1を適当数足すことで必ず product-sum にできる。1の数は \[積 - 1以外の項の和\] */
  case class ProductSum(product: Int, sumWithoutOne: Int, numOfTermsWithoutOne: Int, lastTerm: Int) {
    def +(e: Int) = ProductSum(product * e, sumWithoutOne + e, numOfTermsWithoutOne + 1, e)

    def k = product - sumWithoutOne + numOfTermsWithoutOne
  }

  for {
    e <- Iterator.from(2).takeWhile(e => e * e - 2 * e + 2 <= max)
    ps <- productSums(ProductSum(e, e, 1, e)) if ps.numOfTermsWithoutOne > 1
    k = ps.k if k <= max
  } minimals(k) =
    if (minimals(k) == 0) ps.product
    else minimals(k) min ps.product

  println(minimals.distinct.sum)
}
