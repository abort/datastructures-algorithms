import scala.io.StdIn

object Knapsack {
  def main(args: Array[String]): Unit = {
    val line = StdIn.readLine.split(' ').map(_.toInt)
    val capacity = line(0) // ignore 2nd argument, as we don't need it
    val weights = StdIn.readLine.split(' ').map(_.toInt)

    println(computeMaximumWeight(capacity, weights))
  }

  def computeMaximumWeight(capacity : Int, weights : Array[Int]): Int = {
    val knapsackMatrix = Array.fill[Int](capacity + 1, weights.size + 1)(0) // 2D matrix
    for (i <- 1 to weights.size) {
      for (w <- 1 to capacity) {
        val itemWeight = weights(i - 1)
        knapsackMatrix(w)(i) = knapsackMatrix(w)(i - 1)
        if (itemWeight <= w) {
          val potentialValue = knapsackMatrix(w - itemWeight)(i - 1) + itemWeight // itemWeight == value in this case
          if (knapsackMatrix(w)(i) < potentialValue) {
            knapsackMatrix(w)(i) = potentialValue
          }
        }
      }
    }

    knapsackMatrix(capacity)(weights.size)
  }
}
