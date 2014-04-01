package nlp.scala.euler

import scala.math.BigInt

object Worksheet {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val max = 100                                   //> max  : Int = 100
  val tmp = for {
    a <- 2 to max
    b <- 2 to max
  } yield BigInt(a).pow(b)                        //> tmp  : scala.collection.immutable.IndexedSeq[scala.math.BigInt] = Vector(4, 
                                                  //| 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 1
                                                  //| 31072, 262144, 524288, 1048576, 2097152, 4194304, 8388608, 16777216, 3355443
                                                  //| 2, 67108864, 134217728, 268435456, 536870912, 1073741824, 2147483648, 429496
                                                  //| 7296, 8589934592, 17179869184, 34359738368, 68719476736, 137438953472, 27487
                                                  //| 7906944, 549755813888, 1099511627776, 2199023255552, 4398046511104, 87960930
                                                  //| 22208, 17592186044416, 35184372088832, 70368744177664, 140737488355328, 2814
                                                  //| 74976710656, 562949953421312, 1125899906842624, 2251799813685248, 4503599627
                                                  //| 370496, 9007199254740992, 18014398509481984, 36028797018963968, 720575940379
                                                  //| 27936, 144115188075855872, 288230376151711744, 576460752303423488, 115292150
                                                  //| 4606846976, 2305843009213693952, 4611686018427387904, 9223372036854775808, 1
                                                  //| 8446744073709551616, 36893488147419103232, 73786976294838206464, 14757395258
                                                  //| 9676412928, 295147905179
                                                  //| Output exceeds cutoff limit.
  println(tmp.distinct.size.toString)             //> 9183

  def perfectPower(n: Int) = Iterator.from(2).map(BigInt(n).pow(_))
                                                  //> perfectPower: (n: Int)Iterator[scala.math.BigInt]
  def perfectPowers(max: Int) = Iterator.from(2).map(perfectPower(_).takeWhile(_ <= max))
                                                  //> perfectPowers: (max: Int)Iterator[Iterator[scala.math.BigInt]]

  var c = 1                                       //> c  : Int = 1
  for {
    powers <- perfectPowers(max).takeWhile(!_.isEmpty)
  } {
    c = c + 1
    println(powers.size)

  }                                               //> 5
                                                  //| 3
                                                  //| 2
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 1
                                                  //| 1
  

  val result = (max - 1) * (max - 1)              //> result  : Int = 9801

}