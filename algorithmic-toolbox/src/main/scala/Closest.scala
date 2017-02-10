import scala.io.StdIn

object Closest {
  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt
    val points = Array.ofDim[(Int, Int)](n)
    for (i <- 0 until n) {
      val line = StdIn.readLine.split(' ').map(_.toInt)
      points(i) = (line(0), line(1))
    }

    println(computeClosestDistance(points))
  }

  def computeClosestDistance(points: Array[(Int, Int)]): Double = {
    def computeEuclidianDistance(x1: Int, y1: Int, x2: Int, y2: Int): Double = {
      val left = (x1 - x2)
      val right = (y1 - y2)
      val distance = Math.sqrt(left * left + right * right)

      distance
    }

    def computeSequence(points: Array[(Int, Int)], lo : Int, hi : Int): Double = {
      var smallestDistance = Double.PositiveInfinity
      val precomputed = Array.ofDim[(Int, Int)](points.length)
      for ((p, i) <- points zipWithIndex) {

        precomputed(i) = (p._1 * p._1, p._2 * p._2)

        println(s"index $i")

        for (q <- points.slice(i + 1, points.length)) {
          println(":P")

          println(s"\t- $i + 1")

          smallestDistance = Math.min(smallestDistance, computeEuclidianDistance(p, ))
        }
      }
      smallestDistance
    }

    println("jjj")
    val amountToTake = points.length / 2
    val left = points.take(amountToTake)
    val right = points.slice(amountToTake, points.length)
    val computedLeft = computeHalf(left)
    println(left.mkString(", "))
    val computedRight = computeHalf(right)
    println(right.mkString(", "))
    var minDistance = Math.min(computedLeft._1, computedRight._1)
    for ((l, i) <- left.zipWithIndex) {
      val squaredX1 = computedLeft._2(i)._1
      val squaredY1 = computedLeft._2(i)._2
      for (r <- right) {
        val squaredX2 = computedRight._2(i)._1
        val squaredY2 = computedRight._2(i)._1
        minDistance = Math.min(minDistance, computeEuclidianDistance(x1, y1, x2, y2))
      }
    }

    minDistance
  }
}
