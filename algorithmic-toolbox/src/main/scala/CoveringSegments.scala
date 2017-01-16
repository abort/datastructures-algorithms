import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object CoveringSegments {
  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt
    val segments = Array.ofDim[(Int, Int)](n)
    for (i <- 0 until n) {
      val line = StdIn.readLine.split(' ').map(_.toInt)
      segments(i) = (line(0), line(1))
    }

    val computedSegments = computeMinimumSegments(n, segments)
    println(computedSegments.length)
    println(computedSegments.mkString(" "))
  }

  def computeMinimumSegments(n : Int, segments : Array[(Int, Int)]) : Array[Int] = {
    val sortedSegments = segments.sortBy(_._2) // sort by end (minimum -> maximum)
    val computedPoints = ArrayBuffer[Int]()

    var point = sortedSegments.head._2
    computedPoints += point

    for ((start, end) <- sortedSegments.tail) {
      if (point < start || point > end) {
        point = end
        computedPoints += end
      }
    }

    computedPoints.toArray
  }
}
