import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object DifferentSummands {
  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt
    val computedSummands = computeOptimalSummands(n)
    println(computedSummands.length)
    println(computedSummands.mkString(" "))
  }

  def computeOptimalSummands(n : Int) : Array[Int] = {
    val buffer = ArrayBuffer[Int]()

    def computeOptimalSummands(k: Int, l: Int) : Unit = {
      if (k <= 2 * l) {
        buffer += k
      }
      else {
        buffer += l
        computeOptimalSummands(k - l, l + 1) // solve remainder of k, with lowerbound for l
      }
    }

    computeOptimalSummands(n, 1)
    buffer.toArray
  }
}
