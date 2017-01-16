import scala.io.StdIn

object DotProduct {
  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt
    val profitPerClick = StdIn.readLine.split(' ').map(_.toLong)
    val averageNumberOfClicks = StdIn.readLine.split(' ').map(_.toLong)
    println(computeMaximumProfit(n, profitPerClick, averageNumberOfClicks))
  }

  def computeMaximumProfit(n : Int, profitPerClick : Array[Long], averageNumberOfClicks : Array[Long]) : Long = {
    val profitVector = profitPerClick.sortBy(- _)
    val clicksVector = averageNumberOfClicks.sortBy(- _)

    profitVector.zip(clicksVector).map { case (p : Long, c : Long) => p * c }.sum
  }
}
