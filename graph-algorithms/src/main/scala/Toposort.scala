import scala.io.StdIn

object Toposort {
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

    println(sort(edges, inbound).mkString(" "))
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

  private def sort(edges : Edges, inbound : IndexedSeq[Int]) : Seq[Int] = {
    var visited : IndexedSeq[Boolean] = IndexedSeq.fill(edges.size)(false)
    var ordered : Seq[Int] = Seq.empty

    def sort(x : Int): Unit = {
      visited = visited.updated(x, true)
      for (i <- edges(x)) {
        if (!visited(i)) sort(i)
      }

      ordered = x +: ordered
    }

    for (v <- edges.indices.tail) {
      if (!visited(v) && inbound(v) == 0) sort(v)
    }
    for (v <- edges.indices.tail) {
      if (!visited(v) && inbound(v) != 0) sort(v)
    }
    ordered
  }

  private def readLineAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)
}
