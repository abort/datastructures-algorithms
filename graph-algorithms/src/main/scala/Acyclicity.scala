import scala.io.StdIn

object Acyclicity {
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
    }

    println(if (computeCyclicity(edges)) 1 else 0)
  }

  private def computeCyclicity(edges : Edges) : Boolean = {
    def computeCyclicity(x : Int, trace : Set[Int] = Set.empty): Boolean = {
      for (i <- edges(x)) {
        if (trace(i)) return true
        if (computeCyclicity(i, trace + x)) return true
      }

      false
    }

    for (v <- edges.indices.tail) {
      if (computeCyclicity(v)) return true
    }
    false
  }

  private def readLineAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)
}
