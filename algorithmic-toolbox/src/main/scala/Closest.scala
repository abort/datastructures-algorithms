import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Closest {
  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt
    val points = Array.ofDim[(Int, Int)](n)
    for (i <- 0 until n) {
      val line = StdIn.readLine.split(' ').map(_.toInt)
      points(i) = (line(0), line(1))
    }

    println(computeClosestDistance(points.toVector))
  }

  def computeClosestDistance(points: Vector[(Int, Int)]): Double = {
    def computeEuclidianDistance(p : (Int, Int), q : (Int, Int)) : Double = {
      def computeEuclidianDistance(x1 : Int, y1: Int, x2: Int, y2: Int) : Double = {
        val left = x1 - x2
        val right = y1 - y2
        Math.sqrt(left * left + right * right)
      }

      computeEuclidianDistance(p._1, p._2, q._1, q._2)
    }

    def computeMin(strip : Vector[(Int, Int)], min : Double) : Double = {
      var computedMin = min

      val sorted = strip.sortBy(_._2)
      for (i <- 0 to sorted.length) {
        for (j <- i + 1 to sorted.length) {
          if (sorted(j)._2 - sorted(i)._2 >= min)
            return min

          val distance = computeEuclidianDistance(sorted(i), sorted(j))
          computedMin = Math.min(computedMin, distance)
        }
      }
      computedMin
    }

    def compute(points: Vector[(Int, Int)], lo : Int, hi : Int) : Double = {
      val n = hi - lo + 1
      if (n <= 3) {
//        println("Bruteforce")
        var min = Double.PositiveInfinity
        for (i <- lo to hi) {
          for (j <- i + 1 to hi) {
            val p1 = points(i)
            val p2 = points(j)
//            println(s"Computing between $p1 and $p2")
            min = Math.min(min, computeEuclidianDistance(points(i), points(j)))
          }
        }

        return min
      }

//      println("Non bruteforce")
      val virtualMidIndex = lo + n / 2
      val virtualMid = points(virtualMidIndex)

      val minLeft = compute(points, 0, virtualMidIndex)
      val minRight = compute(points, virtualMidIndex, hi)

      val min = Math.min(minLeft, minRight)

      val strip = ArrayBuffer[(Int, Int)]()
      for (i <- lo to hi) {
        val p = points(i)
//        println(s"Iterating strip $i ($lo, $hi) (point: $p)")
        if (Math.abs(points(i)._1 - virtualMid._1) < min) {
          strip.append(points(i))
        }
      }

      Math.min(min, computeMin(strip.toVector, min))
    }

    val sortedPoints = points.sortBy(_._1) // sorted by X coordinate
//    println(sortedPoints.mkString("\n"))
    compute(sortedPoints, 0, sortedPoints.length - 1)
  }
}
