import scala.collection.mutable.{Map, Stack}
import scala.io.StdIn

object PrimitiveCalculator {
  def main(args: Array[String]): Unit = {
    val result = computeSteps(StdIn.readInt)
    println(result._1)
    println(result._2.mkString(" "))
  }

  def computeSteps(n : Int) : (Int, Iterable[Int]) = {
    val stepMap = Map(1 -> 0)

    def computeOptimalSteps(x : Int) = Array(x - 1, x / 3 + 1, x / 2 + 1).map(stepMap.get(_).get).min // gets minimum amount of steps for either one of the options

    // We compute up to n - 1 (n + 1 is the last step we need to compute for n)
    for (i <- 1 until n) {
      val currentStepCount = stepMap.getOrElse(i, computeOptimalSteps(i))
      val newStepCount = currentStepCount + 1

      stepMap(i * 3) = Math.min(stepMap.getOrElse(i * 3, Int.MaxValue), newStepCount)
      stepMap(i * 2) = Math.min(stepMap.getOrElse(i * 2, Int.MaxValue), newStepCount)
      stepMap(i + 1) = Math.min(stepMap.getOrElse(i + 1, Int.MaxValue), newStepCount)
    }

    var m = n
    val trace = Stack(m)
    while (m > 1) {
      val steps = stepMap(m)
      val possiblePreviousSteps = stepMap.filterKeys(Set(m / 3, m / 2, m - 1).contains(_))
      // println(s"Possible previous steps for $m: ${possiblePreviousSteps}")

      // Only take candidates with 1 step less and take the smallest value of those (doesn't matter which one)
      m = possiblePreviousSteps.filter {
        case (k, v) => v == steps - 1
      }.minBy(_._1)._1
      trace.push(m)
    }

    (stepMap(n), trace.toArray)
  }
}
