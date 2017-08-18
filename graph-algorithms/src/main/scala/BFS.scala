import scala.collection.immutable.Queue
import scala.io.StdIn

object BFS {
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

    println(computeMinimumDistance(getVertices(), edges))
  }

  private def computeMinimumDistance(checkVertices: (Int, Int), edges : Edges) : Int = {
    var distances : IndexedSeq[Int] = IndexedSeq.fill[Int](edges.size)(-1)
    val v = checkVertices._1
    val w = checkVertices._2

    distances = distances.updated(v, 0)

    var queue : Queue[Int] = Queue(v)
    while (queue.nonEmpty) {
      val dequeued = queue.dequeue
      var newQueue = dequeued._2
      val node = dequeued._1

      edges(node).foreach{ neighbour =>
        if (distances(neighbour) == -1) {
          // update neighbour distance and add to queue
          distances = distances.updated(neighbour, distances(node) + 1)

          if (neighbour == w) return distances(w)
          newQueue = newQueue.enqueue(neighbour)
        }
      }


      queue = newQueue
    }

    distances(w)
  }

  private def readLineAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)
  private def getVertices() : (Int, Int) = {
    val line = readLineAsInts()
    (line.head, line(1))
  }
}
