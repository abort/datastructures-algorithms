import scala.collection.immutable.Queue
import scala.io.StdIn

object NegativeWeight {
  type Edges = IndexedSeq[IndexedSeq[Int]]
  type Weights = IndexedSeq[IndexedSeq[Int]]

  sealed trait Distance extends Ordered[Distance] {
    def +(that: Int): Distance
  }

  case object Infinite extends Distance {
    override def compare(that: Distance): Int = that match {
      case Infinite => 0
      case Finite(_) => 1
    }

    override def +(that: Int): Distance = Infinite
  }

  case class Finite(value: Int) extends Distance {
    override def compare(that: Distance): Int = that match {
      case Infinite => -1
      case Finite(v) => value compare v
    }

    override def +(that: Int): Distance = Finite(value + that)
  }

  def main(args: Array[String]): Unit = {
    val line = readLineAsInts()
    val n = line.head
    val m = line(1)
    var edges: Edges = IndexedSeq.fill[IndexedSeq[Int]](n + 1)(IndexedSeq.empty)
    var weights: Weights = IndexedSeq.fill[IndexedSeq[Int]](n + 1)(IndexedSeq.empty)
    var inbound: IndexedSeq[Int] = IndexedSeq.fill[Int](n + 1)(0)

    for (_ <- 1 to m) {
      val line = readLineAsInts()
      val v = line.head
      val w = line(1)
      val weight = line(2)

      edges = edges.updated(v, edges(v) :+ w)
      weights = weights.updated(v, weights(v) :+ weight)
      inbound = inbound.updated(w, inbound(w) + 1)
    }

    println(if (computeNegativeCycles(edges, weights, inbound)) 1 else 0)
  }


  private def computeNegativeCycles(edges: Edges, weights: Weights, inbound: IndexedSeq[Int]): Boolean = {
    val components = edges.indices.tail.toSet
    val vertices = edges.size
    var visited = IndexedSeq.fill[Boolean](vertices)(false)
    components.foreach { v =>
      if (!visited(v)) {
        var distances = IndexedSeq.fill[Distance](vertices)(Infinite)
        var queued = IndexedSeq.fill[Boolean](vertices)(false)
        var queue = Queue.empty[Int]
        var cost = 0

        distances = distances.updated(v, Finite(0))
        queue = queue :+ v
        queued = queued.updated(v, true)

        while (queue.nonEmpty) {
          val deq = queue.dequeue
          val u = deq._1
          queue = deq._2
          visited = visited.updated(u, true)
          queued = queued.updated(u, false)

          edges(u).zipWithIndex.foreach { case (w, index) =>
            val computedDistance = distances(u) + weights(u)(index)
            if (distances(w) > computedDistance) {
              distances = distances.updated(w, computedDistance)
              if (!queued(w)) {
                queue = queue :+ w
                queued = queued.updated(w, true)
              }
              cost = cost + 1
              if (cost > vertices) return true
            }
          }
        }
      }
    }

    false
  }

  private def readLineAsInts(): Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)
}