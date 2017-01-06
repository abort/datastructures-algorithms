import scala.io.StdIn

object FibonacciPartialSum {
  def main(args: Array[String]): Unit = {
    while(true) {
      val line = StdIn.readLine().split(' ').map(_.toLong)
      println(fibonacciPartialSum(line(0), line(1)))
    }
  }

  def fibonacciPartialSum(m : Long, n : Long) : Int = {
    if (n <= 1) return n.toInt

    val pisanoPeriod = 60 // 6 * m
    val period = Array.ofDim[Int](pisanoPeriod)
    period(0) = 0
    period(1) = 1
    for (i <- 2 until pisanoPeriod) {
      period(i) = (period(i - 1) + period(i - 2)) % 10
    }

    val indexDelta = n - m
    val startIndex = m % pisanoPeriod
    var virtualEndIndex = startIndex + indexDelta
    val fullPeriodCount = (virtualEndIndex - startIndex) / pisanoPeriod

    val compute = (p : Array[Int]) => p.foldLeft(0) { (a, b) => { (a + b) } } % 10
    val computedFullPeriods = fullPeriodCount * compute(period)
    virtualEndIndex = virtualEndIndex - fullPeriodCount * pisanoPeriod

    var result : Long = computedFullPeriods
    for (i <- startIndex to virtualEndIndex) {
      // Go circularly through the buffer
      result += period((i % pisanoPeriod).toInt) % 10
    }
    (result % 10).toInt
  }
}
