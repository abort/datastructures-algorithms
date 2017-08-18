import scala.collection.immutable.Queue
import scala.io.StdIn

object Bipartite {
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

    println(if (computeIsBipartite(edges)) 1 else 0)
  }

  sealed trait Color
  case object Uncolored extends Color
  case object Black extends Color
  case object White extends Color

  private def computeIsBipartite(edges : Edges) : Boolean = {
    var colors : IndexedSeq[Color] = IndexedSeq.fill[Color](edges.size)(Uncolored)

    var queue : Queue[Int] = Queue(1)
    colors = colors.updated(1, Black)

    while (queue.nonEmpty) {
      val dequeued = queue.dequeue
      var newQueue = dequeued._2
      val node = dequeued._1

      edges(node).foreach{ neighbour =>
        val expectedNeighbourColor = colors(node) match {
          case Black => White
          case White => Black
          case _ => ???
        }

        if (colors(neighbour) == Uncolored) {
          // update neighbour distance and add to queue
          colors = colors.updated(neighbour, expectedNeighbourColor)
          newQueue = newQueue.enqueue(neighbour)
        }
        else if (colors(neighbour) != expectedNeighbourColor) {
          return false
        }
      }


      queue = newQueue
    }

    true
  }

  private def readLineAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)
}
