import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.util
import java.util.StringTokenizer

import scala.collection.mutable.ArrayBuffer

object BuildHeap {
  object InputReader {
    val reader : BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    var tokenizer : StringTokenizer = null

    private def next() : String = {
      while (tokenizer == null || !tokenizer.hasMoreTokens) {
        tokenizer = new StringTokenizer(reader.readLine())
      }
      tokenizer.nextToken()
    }

    def nextInt() : Int = next().toInt
    def nextLong() : Long = next().toLong
  }

  def main(args: Array[String]): Unit = {
    val out = new PrintWriter(new OutputStreamWriter(System.out))
    val n = InputReader.nextInt()
    val items = Array.ofDim[Long](n)
    for (i <- 0 until n) {
      items(i) = InputReader.nextLong()
    }

    val heap = buildHeap(items)
    out.println(heap.size)
    heap.foreach { case (a, b) => out.println(a + " " + b) }
    out.close()
  }

  def buildHeapIterative(items : Array[Long]) : Seq[(Int, Int)] = {
    val heap = items
    val queue = ArrayBuffer[(Int, Int)]()
    val work = new util.Stack[Int]()

    for (i <- 0 until heap.length / 2) {
      work.push(i)
    }

    while (!work.empty()) {
      val i = work.pop()
      var minIndex = i
      val childrenOffset = i * 2 + 1
      if (childrenOffset < heap.length && heap(childrenOffset) < heap(minIndex)) {
        minIndex = childrenOffset
      }
      if (childrenOffset + 1 < heap.length && heap(childrenOffset + 1) < heap(minIndex)) {
        minIndex = childrenOffset + 1
      }

      // in case a child has a lower value
      if (minIndex != i) {
        queue += ((i, minIndex))
        val rootValue = heap(i)

        heap(i) = heap(minIndex)
        heap(minIndex) = rootValue

        // continue sinking the largest swapped element down the heap
        work.push(minIndex)
      }
    }

    queue
  }

  def buildHeap(heap : Array[Long]) : Seq[(Int, Int)] = {
    //val queue = Queue.empty[(Int, Int)]
    val queue = ArrayBuffer[(Int, Int)]()

    // take root nodes
    for (i <- (heap.length / 2 - 1) to 0 by - 1) {
      // sink(heap, i, queue)
      sink(heap, i, queue)
    }

    queue
  }

  def sink(heap: Array[Long], i: Int, queue: ArrayBuffer[(Int, Int)]): Unit = {
    // select minimal element
    var minIndex = i
    val childrenOffset = i * 2 + 1
    if (childrenOffset >= heap.length) return // for speed, but redundant

    if (childrenOffset < heap.length && heap(childrenOffset) < heap(minIndex)) {
      minIndex = childrenOffset
    }
    if (childrenOffset + 1 < heap.length && heap(childrenOffset + 1) < heap(minIndex)) {
      minIndex = childrenOffset + 1
    }

    // in case a child has a lower value
    if (minIndex != i) {
      val tuple = (i, minIndex)
      queue += tuple
      val rootValue = heap(i)
      heap(i) = heap(minIndex)
      heap(minIndex) = rootValue

      // continue sinking the largest swapped element down the heap
      sink(heap, minIndex, queue)
    }
  }
}