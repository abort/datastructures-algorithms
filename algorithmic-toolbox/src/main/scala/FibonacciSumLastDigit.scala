import scala.io.StdIn

object FibonacciSumLastDigit {
  def main(args: Array[String]): Unit = {
    while(true) {
      val n = StdIn.readLong()
      println(fibonacciSumLastDigit(n))
    }
  }

  def fibonacciSumLastDigit(n : Long) : Int = {
    if (n <= 1) return n.toInt

    val pisanoPeriod = 60 // 6 * m
    val period = Array.ofDim[Int](pisanoPeriod)
    period(0) = 0
    period(1) = 1
    for (i <- 2 until pisanoPeriod) {
      period(i) = (period(i - 1) + period(i - 2)) % 10
    }
    //println("Period:")
    //println(period.mkString(", "))

    val compute = (p : Array[Int]) => p.foldLeft(0) { (a, b) => { (a + b) } } % 10
    val fullDivisions = (n / pisanoPeriod).toInt
    var lastDigit : Int = if (n < pisanoPeriod) 0 else fullDivisions * compute(period)
    lastDigit + compute(period.take((n % pisanoPeriod).toInt + 1))
  }
}
