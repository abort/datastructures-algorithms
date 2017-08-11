import scala.io.StdIn

object ConnectedComponents {
  type Edges = IndexedSeq[Seq[Int]]

  def main(args: Array[String]): Unit = {
    val line = readLineAsInts()
    val n = line.head
    val m = line(1)
    var edges : Edges = IndexedSeq.fill[Seq[Int]](n + 1)(Seq.empty)

    for (_ <- 1 to m) {
      val line = readLineAsInts
      val u = line.head
      val w = line(1)

      edges = edges.updated(u, edges(u) :+ w)
      edges = edges.updated(w, edges(w) :+ u)
    }

    println(computeConnectedComponents(edges))
  }

  private def computeConnectedComponents(edges : Edges) : Int = {
    var visited : IndexedSeq[Boolean] = IndexedSeq.fill(edges.size)(false)

    def computeConnectedComponents(x : Int, components : Int): Unit = {
      visited = visited.updated(x, true)

      for (i <- edges(x)) {
        if (!visited(i))
          computeConnectedComponents(i, components)
      }
    }

    var components = 0
    // skip vertex 0
    for (vertex <- 1 until edges.size) {
      if (!visited(vertex)) components += 1
      computeConnectedComponents(vertex, components)
    }

    components
  }

  private def readLineAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)
  private def getVertices() : (Int, Int) = {
    val line = readLineAsInts()
    (line.head, line(1))
  }
}
