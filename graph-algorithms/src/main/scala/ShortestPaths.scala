import scala.collection.immutable.Queue
import scala.io.StdIn

object ShortestPaths {
  type Edges = IndexedSeq[IndexedSeq[Int]]
  type Weights = IndexedSeq[IndexedSeq[Long]]

  sealed trait Distance extends Ordered[Distance] {
    def +(that: Long): Distance
  }

  case object InfinitePositive extends Distance {
    override def compare(that: Distance): Int = that match {
      case InfinitePositive => 0
      case InfiniteNegative => 1
      case Finite(_) => 1
    }

    override def +(that: Long): Distance = InfinitePositive
  }

  case object InfiniteNegative extends Distance {
    override def compare(that: Distance): Int = that match {
      case InfinitePositive => -1
      case InfiniteNegative => 0
      case Finite(_) => -1
    }

    override def +(that: Long): Distance = InfiniteNegative
  }

  case class Finite(value: Long) extends Distance {
    override def compare(that: Distance): Int = that match {
      case InfinitePositive => -1
      case InfiniteNegative => 1
      case Finite(v) => value compare v
    }

    override def +(that: Long): Distance = Finite(value + that)
  }

  trait Result
  case object NoPath extends Result
  case object PathOnNegativeCycle extends Result
  case class ShortenedPath(distance : Long) extends Result

  def main(args: Array[String]): Unit = {
    val line = readLineAsInts()
    val n = line.head
    val m = line(1)
    var edges: Edges = IndexedSeq.fill[IndexedSeq[Int]](n + 1)(IndexedSeq.empty)
    var weights: Weights = IndexedSeq.fill[IndexedSeq[Long]](n + 1)(IndexedSeq.empty)

    for (_ <- 1 to m) {
      val line = readLineAsInts()
      val v = line.head
      val w = line(1)
      val weight = line(2)

      edges = edges.updated(v, edges(v) :+ w)
      weights = weights.updated(v, weights(v) :+ weight.toLong)
    }

    computeShortestPaths(StdIn.readInt, edges, weights).foreach { p =>
      println(p match {
        case NoPath => "*"
        case ShortenedPath(v) => v.toString
        case PathOnNegativeCycle => "-"
      })
    }
  }


  private def computeShortestPaths(v: Int, edges: Edges, weights: Weights): Seq[Result] = {
    val vertices = edges.size

    var distances = IndexedSeq.fill[Distance](vertices)(InfinitePositive)
    var relaxed = IndexedSeq.fill[Boolean](vertices)(false)
    var queued = IndexedSeq.fill[Boolean](vertices)(false)
    var parents = IndexedSeq(edges.indices :_ *)
    var queue = Queue(v)
    var cycles = Set.empty[Int]
    var cost = 0

    distances = distances.updated(v, Finite(0))
    queued = queued.updated(v, true)

    while (queue.nonEmpty) {
      val deq = queue.dequeue
      val u = deq._1
      queue = deq._2
      queued = queued.updated(u, false)

      edges(u).zipWithIndex.foreach { case (w, index) =>
        val computedDistance = distances(u) + weights(u)(index)
        if (distances(w) > computedDistance) {
          // Relaxed check
          if (distances(w) == InfinitePositive) {
            relaxed = relaxed.updated(w, true)
          }
          distances = distances.updated(w, computedDistance)
          parents = parents.updated(w, u)
          if (!queued(w)) {
            queue = queue :+ w
            queued = queued.updated(w, true)
            cost = cost + 1
            if (cost > vertices) {
              // w is part of a negative cycle
              var cycleNode = w
              var cycle = Set(w)
              for (i <- 0 to vertices) {
                cycleNode = parents(cycleNode)
                cycle = cycle + cycleNode
              }
              cycle.foreach(i => distances = distances.updated(i, InfiniteNegative))
              cycles = cycles ++ cycle
            }
          }
        }
      }
    }

    // BFS
    var visited = IndexedSeq.fill[Boolean](vertices)(false)
    var bfsQueue : Queue[Int] = Queue(cycles.toSeq : _*)
    while (bfsQueue.nonEmpty) {
      val r = bfsQueue.dequeue
      val u = r._1
      bfsQueue = r._2
      if (!visited(u)) {
        visited = visited.updated(u, true)

        edges(u).foreach { w =>
          if (!visited(w)) {
            distances = distances.updated(w, InfiniteNegative)
            bfsQueue = bfsQueue.enqueue(w)
          }
        }
      }
    }



    distances.zipWithIndex.tail.map { case (distance, index) => distance match {
        //case Finite(_) if cycles.contains(index) => PathOnNegativeCycle
        case Finite(value) => ShortenedPath(value)
        case InfinitePositive => NoPath
        case InfiniteNegative => PathOnNegativeCycle
      }
    }
  }

  private def readLineAsInts(): Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)
}