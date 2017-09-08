import scala.annotation.tailrec
import scala.io.StdIn
import scala.math.Ordering
import scala.reflect.ClassTag

object ConnectingPoints {
  type Edges = Map[Point, Set[Point]]

  case class Point(x : Int, y  : Int) {
    def -(that: Point) : Double = Math.sqrt(Math.pow(x - that.x, 2) + Math.pow(y - that.y, 2))
  }

  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt
    var allPoints = Set.empty[Point]

    for (_ <- 1 to n) {
      val line = readLineAsInts()
      allPoints = allPoints + Point(line.head, line(1))
    }

    println(computeOptimalTotalLength(allPoints.map(p => (p, allPoints)).toMap))
  }


  private def computeOptimalTotalLength(edges: Edges) : Double = {
    val indexedVertices = edges.keys.toIndexedSeq
    var cost = edges.map { case (k, _) => (k, Double.PositiveInfinity) }
    var visited = edges.map { case (k, _) => (k, false) }
    val pointToIndexMap = indexedVertices.zipWithIndex.toMap

    // start index
    val queue = new IndexMinPQ[Double](indexedVertices.size)
    val u = 0
    cost = cost + (indexedVertices(u) -> 0.0)
    queue.insert(u, 0.0)

    while (!queue.isEmpty) {
      val v = queue.delMin()

      visited = visited.updated(indexedVertices(v), true)
      edges(indexedVertices(v)).foreach { w =>
        if (!visited(w)) {
          val currentDistance = Math.abs(w - indexedVertices(v))
          if (currentDistance < cost(w)) {
            cost = cost.updated(w, currentDistance)
            val index = pointToIndexMap(w)
            if (queue.contains(index)) queue.changeKey(index, cost(w))
            else queue.insert(index, cost(w))
          }
        }
      }
    }
    cost.map { case (_, v) => v }.sum
  }

  private def readLineAsInts() : Seq[Int] = StdIn.readLine.trim.split(" ").map(_.toInt)

  /** Common code for IndexMinPQ, IndexMaxPQ
    *
    * @author Scala translation by Gary Struthers from Java by Robert Sedgewick and Kevin Wayne.
    *
    * @constructor called by subclass
    * @tparam A keys are generic, ClassTag retains its type at runtime
    * @param nMax maximum number of elements
    * @param ord implicit ordering
    */
  abstract class IndexPriorityQueue[A: ClassTag](nMax: Int)(implicit ord: Ordering[A]) {
    require(nMax >= 0, s"nMax:$nMax can't be negative")
    private var n = 0
    private val keys = new Array[A](nMax + 1)
    private val pq = new Array[Int](keys.size)
    private val qp = Array.fill[Int](keys.size)(-1)

    def isEmpty: Boolean = n == 0

    protected def less(a: Int, b: Int): Boolean =
      ord.lt(keys(pq(a)), keys(pq(b)))

    protected def greater(a: Int, b: Int): Boolean =
      ord.gt(keys(pq(a)), keys(pq(b)))

    private def rangeGuard(x: Int): Boolean = x match {
      case x if 0 to nMax contains x => true
      case _ => false
    }

    def contains(i: Int): Boolean = {
      require(rangeGuard(i), s"i:$i not in 0 - nMax:$nMax")
      qp(i) != -1
    }

    def size(): Int = n

    private def exchange(i: Int, j: Int): Unit = {
      val swap = pq(i)
      pq.update(i, pq(j))
      pq.update(j, swap)
      qp(pq(j)) = j
      qp(pq(i)) = i
    }

    private def swim(k: Int, cmp: (Int, Int) => Boolean): Unit = {
      @tailrec
      def loop(i: Int, j: Int) {
        if (i > 1 && cmp(j, i)) {
          exchange(i, j)
          loop(j, j / (2))
        }
      }
      loop(k, k./(2))
    }

    private def sink(k: Int, cmp: (Int, Int) => Boolean): Unit = {
      @tailrec
      def loop(k: Int): Unit = {
        def calcJ(): Int = {
          val j = k * 2
          val j1 = j + 1
          if ((j1 <= n) && cmp(j, j1)) j1 else j
        }
        val j = calcJ
        if (j <= n && cmp(k, j)) {
          exchange(k, j)
          loop(j)
        }
      }
      loop(k)
    }

    protected def insert(i: Int, key: A, cmp: (Int, Int) => Boolean): Unit = {
      require(rangeGuard(i), s"i:$i not in 0 - nMax:$nMax")
      require(!contains(i), s"index:$i is already in the priority queue")
      n += 1
      qp(i) = n
      pq(n) = i
      keys(i) = key
      swim(n, cmp)
    }

    protected def index(): Int = {
      require(n > 0, s"n:$n priority queue underflow")
      pq(1)
    }

    protected def topKey(): A = {
      require(n > 0, s"n:$n priority queue underflow")
      keys(pq(1))
    }

    protected def delTop(cmp: (Int, Int) => Boolean): Int = {
      require(n > 0, s"n:$n priority queue underflow")
      val top = pq(1)
      exchange(1, n)
      n -= 1
      sink(1, cmp)
      qp(top) = -1
      pq(n + 1) = -1
      top
    }

    def keyOf(i: Int): A = {
      require(rangeGuard(i), s"i:$i not in 0 - nMax:$nMax")
      require(contains(i), s"index:$i is not in the priority queue")
      keys(i)
    }

    protected def changeKey(i: Int, key: A, cmp: (Int, Int) => Boolean): Unit = {
      require(rangeGuard(i), s"i:$i not in 0 - nMax:$nMax")
      require(contains(i), s"index:$i is not in the priority queue")
      keys(i) = key
      swim(qp(i), cmp)
      sink(qp(i), cmp)
    }

    protected def decreaseKey(i: Int, key: A, cmp: (Int, Int) => Boolean): Unit = {
      require(rangeGuard(i), s"i:$i not in 0 - nMax:$nMax")
      require(contains(i), s"index:$i is not in the priority queue")
      require(ord.compare(keys(i), key) > 0,
        s"Calling decreaseKey() with i:$i, key:$key would not strictly decrease the key")
      keys(i) = key
      swim(qp(i), cmp)
    }

    protected def increaseKey(i: Int, key: A, cmp: (Int, Int) => Boolean): Unit = {
      val r = ord.compare(keys(i), key)
      require(rangeGuard(i), s"i:$i not in 0 - nMax:$nMax")
      require(contains(i), s"index:$i is not in the priority queue")
      require(r < 0,
        s"Calling increaseKey() with i:$i, key:$key would not strictly increase the key")
      keys(i) = key
      sink(qp(i), cmp)
    }

    protected def delete(i: Int, cmp: (Int, Int) => Boolean): Unit = {
      require(rangeGuard(i), s"i:$i not in 0 - nMax:$nMax")
      require(contains(i), s"index:$i is not in the priority queue")
      val index = qp(i)
      exchange(index, n)
      n -= 1
      swim(index, cmp)
      sink(index, cmp)
      keys(i) = null.asInstanceOf[A]
      qp(i) = -1
    }

    /** returns string of keys */
    override def toString(): String = {
      val sb = new StringBuilder()
      val size = pq.size
      for (i <- 1 until size) {
        val key = keys(pq(i))
        if (key != null) sb append(s" $key")
      }
      sb.toString.trim
    }

    /** returns keys */
    protected def getKeys(): IndexedSeq[A] = for (i <- 1 until nMax) yield keys(pq(i))

    /** check parent in position has left child at k * 2, right child at k * 2 + 1 */
    def checkHeap(cmp: (Int, Int) => Boolean): Boolean = {

      def loop(k: Int): Boolean = {
        if (k > n) true else {
          val left = 2 * k
          val right = 2 * k + 1
          if ((left <= n && cmp(k, left)) || (right <= n && cmp(k, right))) false
          else loop(left) && loop(right)
        }
      }
      loop(1)
    }
  }
  /** Minimum priority queue with index
    *
    * @author Scala translation by Gary Struthers from Java by Robert Sedgewick and Kevin Wayne.
    *
    * @constructor creates a new IndexMinPQ with maximum number of elements
    * @tparam A keys are generic and ordered
    * @param nMax maximum number of elements
    * @param ord implicit ordering
    */
  class IndexMinPQ[A: ClassTag](nMax: Int)(implicit ord: Ordering[A])
    extends IndexPriorityQueue[A](nMax) {

    def insert(i: Int, key: A): Unit = insert(i, key, greater)
    def minIndex(): Int = index
    def minKey(): A = topKey
    def delMin(): Int = delTop(greater)
    def changeKey(i: Int, key: A): Unit = changeKey(i, key, greater)
    def decreaseKey(i: Int, key: A): Unit = decreaseKey(i, key, greater)
    def increaseKey(i: Int, key: A): Unit = increaseKey(i, key, greater)
    def delete(i: Int): Unit = delete(i, greater)
    def isMinHeap(): Boolean = checkHeap(greater)
    def keys(): Seq[A] = getKeys
  }
}
