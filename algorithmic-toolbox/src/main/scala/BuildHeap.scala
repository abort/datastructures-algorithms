import java.io.{OutputStreamWriter, PrintWriter}
import java.util

import scala.collection.mutable.Queue
import scala.io.Source

object BuildHeap {
  def main(args: Array[String]): Unit = {
    val out = new PrintWriter(new OutputStreamWriter(System.out))


    val input = Source.stdin.getLines().take(2).toSeq
    val items = input(1).mkString.split(' ').map(_.toInt)

    val heap = buildHeapIterative(items)
    out.println(heap.size)
    heap.foreach { case (a, b) => out.println(a + " " + b) }
    out.close()
  }

  def buildHeapIterative(items : Array[Int]) : Queue[(Int, Int)] = {
    val heap = 0 +: items
    val queue = Queue.empty[(Int, Int)]
    val work = new util.Stack[Int]()

    // take root nodes
    for (i <- 1 to (heap.length / 2)) {
      //sink(heap, i, queue)
      work.push(i)
    }

    while (!work.empty()) {
      val i = work.pop()
      var minIndex = i
      val childrenOffset = i * 2
      if (childrenOffset < heap.length && heap(childrenOffset) < heap(minIndex)) {
        minIndex = childrenOffset
      }
      if (childrenOffset + 1 < heap.length && heap(childrenOffset + 1) < heap(minIndex)) {
        minIndex = childrenOffset + 1
      }

      // in case a child has a lower value
      if (minIndex != i) {
        queue.enqueue((i - 1, minIndex - 1))
        val rootValue = heap(i)

        heap(i) = heap(minIndex)
        heap(minIndex) = rootValue

        // continue sinking the largest swapped element down the heap
        work.push(minIndex)
      }
    }

    queue
  }

  def buildHeap(items : Array[Int]) : Queue[(Int, Int)] = {
    val heap = 0 +: items
    val queue = Queue.empty[(Int, Int)]
    val work = new util.Stack[Int]()

    // take root nodes
    for (i <- (heap.length / 2) to 1 by - 1) {
      sink(heap, i, queue)
    }

    queue
  }

  def sink(heap: Array[Int], i: Int, queue: Queue[(Int, Int)]): Unit = {
    if (i >= heap.length - 1 || i * 2 >= heap.length) return

    // select minimal element
    val childrenOffset = i * 2
    var minIndex = i
    if (childrenOffset < heap.length && heap(childrenOffset) < heap(minIndex)) {
      minIndex = childrenOffset
    }
    if (childrenOffset + 1 < heap.length && heap(childrenOffset + 1) < heap(minIndex)) {
      minIndex = childrenOffset + 1
    }

    // in case a child has a lower value
    if (minIndex != i) {
      queue.enqueue((i - 1, minIndex - 1))
      val rootValue = heap(i)
      heap(i) = heap(minIndex)
      heap(minIndex) = rootValue

      // continue sinking the largest swapped element down the heap
      sink(heap, minIndex, queue)
    }
  }
}