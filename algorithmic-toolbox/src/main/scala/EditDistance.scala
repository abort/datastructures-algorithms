import scala.io.StdIn

object EditDistance {
  def main(args: Array[String]): Unit = {
    println(computeEditDistance(StdIn.readLine, StdIn.readLine))
  }

  def computeEditDistance(a : String, b : String): Int = {
    val matrix = Array.ofDim[Int](a.length + 1, b.length + 1)

    for (i <- matrix.indices) matrix(i)(0) = i
    for (j <- matrix(0).indices) matrix(0)(j) = j

    for (i <- 1 to a.length) {
      for (j <- 1 to b.length) {
        val insertionCost = matrix(i)(j - 1) + 1
        val deletionCost = matrix(i - 1)(j) + 1
        if (a.charAt(i - 1) == b.charAt(j - 1)) {
          val matchCost = matrix(i - 1)(j - 1)
          matrix(i)(j) = Math.min(Math.min(insertionCost, deletionCost), matchCost)
        }
        else {
          val mismatchCost = matrix(i - 1)(j - 1) + 1
          matrix(i)(j) = Math.min(Math.min(insertionCost, deletionCost), mismatchCost)
        }
      }
    }

    matrix(a.length)(b.length)
  }
}
