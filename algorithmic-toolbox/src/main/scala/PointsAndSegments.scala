import scala.io.StdIn

object PointsAndSegments {
  def main(args: Array[String]): Unit = {
    val line = StdIn.readLine.split(' ').map(_.toInt)
    val amountOfSegments = line(0)
    val amountOfPoints = line(1)
    val segments = Array.ofDim[(Int, Int)](amountOfSegments)
    for (i <- 0 until amountOfSegments) {
      val segmentLine = StdIn.readLine.split(' ').map(_.toInt)
      segments(i) = (segmentLine(0), segmentLine(1))
    }
    val points = StdIn.readLine.split(' ').map(_.toInt)

    assert(points.length == amountOfPoints)

    println(computePrizes(segments, points).mkString(" "))
  }


  def computePrizes(segments : Array[(Int, Int)], points : Array[Int]) : Array[Int] = {
    // Create tuples
    val sortedTuples = createSortedTuples(segments, points)
    val computedPrizes = Array.ofDim[Int](points.length)
    var i = 0
    var currentSegmentCount = 0
    for (e <- sortedTuples) {
      if (e._2 == 'l') currentSegmentCount += 1
      else if (e._2 == 'p') {
        // Store back at original position
        computedPrizes(e._3) = currentSegmentCount
        i += 1
      }
      else if (e._2 == 'r') currentSegmentCount -= 1
    }

    computedPrizes
  }

  def createSortedTuples(segments : Array[(Int, Int)], points : Array[Int]) : Array[(Int, Char, Int)] = {
    def sort(x: (Int, Char, Int), y: (Int, Char, Int)): Boolean = {
      val typeLeft = x._2
      val typeRight = y._2
      val positionLeft = x._1
      val positionRight = y._1

      if (positionLeft != positionRight) positionLeft < positionRight
      else typeLeft < typeRight // Use alphabet for l, p, r sort <3
    }

    // N-Tuples of value, type, original position
    val consolidatedScale = Array.ofDim[(Int, Char, Int)](segments.length * 2 + points.length)
    var i = 0
    for (segment <- segments) {
      consolidatedScale(i) = (segment._1, 'l', 0)
      i += 1
      consolidatedScale(i) = (segment._2, 'r', 0)
      i += 1
    }

    for (p <- points.indices) {
      consolidatedScale(i) = (points(p), 'p', p)
      i += 1
    }

//    val sorted = consolidatedScale.sortWith(sort)
//    println(sorted.mkString(" "))
//    sorted
    consolidatedScale.sortWith(sort)
  }
}
