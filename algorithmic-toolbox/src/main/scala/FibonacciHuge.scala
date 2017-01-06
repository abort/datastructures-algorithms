import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object FibonacciHuge {
  def main(args: Array[String]): Unit = {
    val line = StdIn.readLine().split(' ')
    println(fibonacciHugeModulo(BigInt(line(0)), line(1).toInt))
  }

  def fibonacciHugeModulo(n : BigInt, m : Int) : BigInt = {
    assert(m >= 2)
    val (p, s) = computeSequenceAndPeriodLength(m)
    s((n % p).toInt)
  }

  def computeSequenceAndPeriodLength(m : Int) : (Int, Array[BigInt]) = {
    // TODO optimize by heuristics about period size (no need for mutable arrays then)
    val periodVector = ArrayBuffer[BigInt](0, 1)
    var i = 2
    var looping = true
    while (looping) {
      periodVector += (periodVector(i - 1) + periodVector(i - 2)) % m

      i += 1
      if (periodVector(periodVector.length - 2) == 0 && periodVector.last == 1) {
        looping = false
      }
    }
    (periodVector.length - 2, periodVector.dropRight(2).toArray)
  }
}
