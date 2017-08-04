import scala.io.StdIn

object TreeOrders {
  private var indicesToNodes : Array[Node] = null // ugly but faster than linked list

  case class Node(key : Int, leftIndex : Int, rightIndex : Int) {
    def hasLeft : Boolean = leftIndex != -1
    def hasRight : Boolean = rightIndex != -1

    def left: Option[Node] = if (hasLeft) Some(indicesToNodes(leftIndex)) else None
    def right: Option[Node] = if (hasRight) Some(indicesToNodes(rightIndex)) else None


    def inOrder(f: Int => Unit) : Unit = {
      left.map(_.inOrder(f))
      f(key)
      right.map(_.inOrder(f))
    }

    def preOrder(f: Int => Unit) : Unit = {
      f(key)
      left.map(_.preOrder(f))
      right.map(_.preOrder(f))
    }

    def postOrder(f: Int => Unit) : Unit = {
      left.map(_.postOrder(f))
      right.map(_.postOrder(f))
      f(key)
    }
  }

  def runTreeOrders : Unit = {
    val vertices = StdIn.readInt
    if (vertices == 0) return

    indicesToNodes = Array.ofDim[Node](vertices)
    for (i <- 0 until vertices) {
      val line = StdIn.readLine.trim.split(' ').map(_.toInt)
      val (key, leftIndex, rightIndex) = (line(0), line(1), line(2))
      indicesToNodes(i) = Node(key, leftIndex, rightIndex)
    }

    val root = indicesToNodes(0)
    val work = Array.ofDim[Int](3 * vertices)
    var workIndex = 0
    def add(x : Int) = {
      work(workIndex) = x
      workIndex += 1
    }
    root.inOrder(add)
    root.preOrder(add)
    root.postOrder(add)
    for (i <- 0 to 3) {
      println(work.slice(i * vertices, i * vertices + vertices).mkString(" "))
    }
  }

  def main(args: Array[String]): Unit = {
    new Thread(null, new Runnable() {
      def run() {
        try runTreeOrders
        catch {
          case _ : Throwable =>
        }
      }
    }, "", 1 << 26).start()
  }
}
