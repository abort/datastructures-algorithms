import java.util.{Collections, PriorityQueue}

import scala.io.StdIn

object BurrowsWheelerTransform {
  def main(args: Array[String]): Unit = println(computeBurrowsWheelerTransform(StdIn.readLine))

  def computeBurrowsWheelerTransform(s : String) : String = {
    val cycles = computeCycles(s)
    val sb = new StringBuilder(s.length)
    while (!cycles.isEmpty) {
      val end = cycles.remove()
      sb.insert(0, end.last)
    }
    sb.toString
  }

  @inline private def computeCycles(s : String) : PriorityQueue[String] = {
    val queue = new PriorityQueue[String](s.length, Collections.reverseOrder[String])
    s.indices.foreach(i => queue.add(s.substring(i) + s.substring(0, i)))
    queue
  }
}
