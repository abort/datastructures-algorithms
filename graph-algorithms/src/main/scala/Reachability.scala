import scala.io.StdIn

object Reachability {
  type Edges = IndexedSeq[Seq[Int]]

  def main(args: Array[String]): Unit = {
    val line = readLineAsInts()
    val n = line.head
    val m = line(1)
    var edges : Edges = IndexedSeq.fill[Seq[Int]](n + 1)(Seq.empty)

    for (_ <- 1 to m) {
      val line = readLineAsInts()
      val u = line.head
      val w = line(1)

      edges = edges.updated(u, edges(u) :+ w)
      edges = edges.updated(w, edges(w) :+ u)
    }

    val reachable = checkReachability(getVertices(), edges)
    println(if (reachable) 1 else 0)
  }

  private def checkReachability(checkVertices: (Int, Int), edges : Edges) : Boolean = {
    var visited : IndexedSeq[Boolean] = IndexedSeq.fill(edges.size)(false)
    val v = checkVertices._1
    val w = checkVertices._2

    def checkReachability(x : Int): Boolean = {
      visited = visited.updated(x, true)

      if (x == w) return true

      for (i <- edges(x)) {
        if (!visited(i) && checkReachability(i)) return true
      }

      false
    }

    checkReachability(v)
  }

  private def readLineAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)
  private def getVertices() : (Int, Int) = {
    val line = readLineAsInts()
    (line.head, line(1))
  }
}
