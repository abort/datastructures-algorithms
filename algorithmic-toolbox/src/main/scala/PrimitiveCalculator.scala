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
    // We assume we will never reach max value, options would have been nicer (or doubles with infinite), but this has better performance
    val stepMap = Array.fill[Int](n + 1)(Int.MaxValue)
    stepMap(1) = 0

    def computeOptimalStepCount(x : Int) = {
      // possible memoized options to use (comprehension might slow us down, but whatever)
      val seq = (x - 1) +: (for (i <- 2 to 3 if x % i == 0) yield (x / i))
      // gets minimum amount of steps for either one of the options
      seq.map(stepMap(_)).min
    }
    def precomputeStep(i : Int, value : Int) = if (i <= n) stepMap(i) = Math.min(stepMap(i), value)

    // We compute up to n
    for (i <- 1 to n) {
      val currentOptimum = stepMap(i)
      val optimum = if (currentOptimum != Int.MaxValue) currentOptimum else (computeOptimalStepCount(i) + 1)
      stepMap(i) = optimum

      // Precompute (not necessary for correct answers, but just to save time)
      precomputeStep(i * 3, optimum + 1)
      precomputeStep(i * 2, optimum + 1)
      precomputeStep(i + 1, optimum + 1)
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