import scala.io.StdIn

object StronglyConnected {
  type Edges = IndexedSeq[Seq[Int]]

  def main(args: Array[String]): Unit = {
    val line = readLineAsInts()
    val n = line.head
    val m = line(1)
    var edges : Edges = IndexedSeq.fill[Seq[Int]](n + 1)(Seq.empty)
    var inbound : IndexedSeq[Int] = IndexedSeq.fill[Int](n + 1)(0)

    for (_ <- 1 to m) {
      val line = readLineAsInts()
      val u = line.head
      val w = line(1)

      edges = edges.updated(u, edges(u) :+ w)
      inbound = inbound.updated(w, inbound(w) + 1)
    }

    println(computeStronglyConnectedComponents(edges, inbound))
  }

  private def reverse(edges: Edges) : Edges = {
    var reversed = IndexedSeq.fill[Seq[Int]](edges.size)(Seq.empty)
    edges.zipWithIndex.foreach {
      case (connectedNodes, v) => connectedNodes.foreach {
        w => reversed = reversed.updated(w, reversed(w) :+ v)
      }
    }
    reversed
  }

  private def computeStronglyConnectedComponents(edges : Edges, inbound : IndexedSeq[Int]) : Int = {
    def empty() : IndexedSeq[Boolean] = IndexedSeq.fill(edges.size)(false)

    var visited : IndexedSeq[Boolean] = empty()
    var ordered : Seq[Int] = Seq.empty
    val reversed : Edges = reverse(edges)

    def sort(x : Int) : Unit = {
      visited = visited.updated(x, true)
      for (i <- reversed(x)) {
        if (!visited(i)) sort(i)
      }

      ordered = x +: ordered
    }

    def computeConnections(v : Int) : Unit = {
      visited = visited.updated(v, true)

      for (i <- edges(v)) {
        if (!visited(i))
          computeConnections(i)
      }
    }

    for (v <- reversed.indices.tail) {
      if (!visited(v) && inbound(v) == 0) sort(v)
    }
    for (v <- reversed.indices.tail) {
      if (!visited(v) && inbound(v) != 0) sort(v)
    }

    var components = 0
    visited = empty()
    for (v <- ordered) {
      if (!visited(v)) {
        computeConnections(v)
        components = components + 1
      }
    }
    components
  }

  private def readLineAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)
}
