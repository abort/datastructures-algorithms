import scala.collection.mutable.Stack
import scala.io.StdIn

object PrimitiveCalculator {
  def main(args: Array[String]): Unit = {
    val result = computeSteps(StdIn.readInt)

    // Optimal :)
    println(result._1 + result._2.mkString("\n", " ", ""))
  }

  def computeSteps(n : Int) : (Int, Iterable[Int]) = {
    // We do not need a map, because we know we will pass all integers (thanks to i + 1), hash code computations slow us down for nothing
    // We do not use stepMap(0)
    // We use n value to denote uninitialized, for performance reasons
    val stepMap = Array.fill[Int](n + 1)(n)
    stepMap(1) = 0

    // We distinguish both cases for more performance
    def precomputeStep(i : Int, value : Int) = if (i <= n) stepMap(i) = Math.min(stepMap(i), value)
    def storePrecomputedStep(i: Int, value: Int) = stepMap(i) = Math.min(stepMap(i), value)

    // We compute up to n
    for (i <- 1 until n) {
      val nextOptimum = stepMap(i) + 1

      precomputeStep(i * 3, nextOptimum)
      precomputeStep(i * 2, nextOptimum)
      // Differentiation for performance reasons
      storePrecomputedStep(i + 1, nextOptimum)
    }

    // Build trace
    val trace = Stack(n)
    while (trace.head > 1) {
      val m = trace.head
      val steps = stepMap(m)

      // Only take candidates with 1 step less and take the smallest value of those (doesn't matter which one), we get tuples of value -> amount of steps
      val step = Seq(m / 3, m / 2, m - 1).map(v => (v, stepMap(v))).filter {
        case (k, v) => v == steps - 1
      }.head._1
      trace.push(step)
      // We do not need to take the minimum here due to the invariant of always having an optimum
    }

    (stepMap(n), trace)
  }
}