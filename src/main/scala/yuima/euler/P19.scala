package yuima.euler

object P19 extends App {
  /**
   * You are given the following information, but you may prefer to do some research for yourself.
   * 1 Jan 1900 was a Monday.
   * Thirty days has September,
   * April, June and November.
   * All the rest have thirty-one,
   * Saving February alone,
   * Which has twenty-eight, rain or shine.
   * And on leap years, twenty-nine.
   * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
   * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
   */
  val mDays = List(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  /**(y,m,d) = (年,月,曜日)：各月１日の曜日を調べる*/
  def day(ymd: (Int, Int, Int)) = (ymd) match {
    case (y, 2, day) if y % 400 == 0 => (y, 3, (29 + day) % 7)
    case (y, 2, day) if y % 100 == 0 => (y, 3, (28 + day) % 7)
    case (y, 2, day) if y % 4 == 0 => (y, 3, (29 + day) % 7)
    case (y, 12, day) => (y + 1, 1, (31 + day) % 7)
    case (y, m, day) => (y, m + 1, (mDays(m - 1) + day) % 7)
  }

  // 1900年１月１日は月曜日。1901年から2000年までで、１日が日曜の月の数を得る。
  def solution0 = Iterator.iterate((1900, 1, 1))(day).dropWhile(_._1 < 1901).takeWhile(_._1 < 2001).filter(_._3 == 0).length
  println(solution0)
}