import scala.io.StdIn

object FractionalKnapsack {
  def main(args: Array[String]): Unit = {
    val readLine = () => StdIn.readLine().split(' ').map(_.toInt)

    val line = readLine()
    val n = line(0)
    val capacity = line(1)

    //println(s"lines to read: $n with capacity: $capacity")
    val weights = Array.ofDim[Int](n)
    val values = Array.ofDim[Int](n)
    for (i <- 0 until n) {
      val line = readLine()
      values(i) = line(0)
      weights(i) = line(1)
    }
    println(computeOptimalSolution(n, capacity, values, weights))
  }

  def computeOptimalSolution(n : Int, capacity : Int, values : Array[Int], weights : Array[Int]) : Double = {
    val inventory = values zip weights filterNot(_._2 == 0)
    val ratios = inventory.map { case (a, b) => a.toDouble / b }.zipWithIndex.sortBy(- _._1)
    // End up with a descending sorted array of tuples (ratio, oldIndex)
    var value = 0.0
    var capacityLeft = capacity
    for ((ratio, i) <- ratios) {
      if (capacityLeft == 0) {
        return value
      }
      val weightSelected = weights(i) min capacityLeft
      val computedValue = ratio * weightSelected
      value = value + computedValue
      //println(s"Tuple: ($ratio, $i), Weight selected: $weightSelected, Computed value: $computedValue")
      capacityLeft = capacityLeft - weightSelected
    }
    value
  }
}
