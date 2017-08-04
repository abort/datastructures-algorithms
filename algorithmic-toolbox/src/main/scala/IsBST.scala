import scala.io.StdIn

object IsBST {
  private var indicesToNodes : Array[Node] = null // ugly but faster than linked list

  sealed trait Node {
    def isBinarySearchTree(minAllowed : Option[Int] = None, maxAllowed : Option[Int] = None) : Boolean
  }
  case object Sentinel extends Node {
    override def isBinarySearchTree(minAllowed : Option[Int] = None, maxAllowed : Option[Int] = None): Boolean = true
  }
  case class KeyNode(key : Int, leftIndex : Int, rightIndex : Int) extends Node {
    def hasLeft : Boolean = leftIndex != -1
    def hasRight : Boolean = rightIndex != -1

    def left: Node = if (hasLeft) indicesToNodes(leftIndex) else Sentinel
    def right: Node = if (hasRight) indicesToNodes(rightIndex) else Sentinel

    override def isBinarySearchTree(minAllowed : Option[Int], maxAllowed : Option[Int]) : Boolean = {
      if (minAllowed.isDefined && key < minAllowed.get) return false
      if (maxAllowed.isDefined && key >= maxAllowed.get) return false
      else return left.isBinarySearchTree(minAllowed, Some(key)) && right.isBinarySearchTree(Some(key), maxAllowed)
    }
  }

  def runBinarySeachTreeCheck : Unit = {
    val vertices = StdIn.readInt

    indicesToNodes = Array.ofDim[Node](vertices)
    for (i <- 0 until vertices) {
      val line = StdIn.readLine.trim.split(' ').map(_.toInt)
      val (key, leftIndex, rightIndex) = (line(0), line(1), line(2))
      indicesToNodes(i) = KeyNode(key, leftIndex, rightIndex)
    }

    val root = if (vertices == 0) Sentinel else indicesToNodes(0)
    println(if (root.isBinarySearchTree()) "CORRECT" else "INCORRECT")
  }

  def main(args: Array[String]): Unit = {
    new Thread(null, new Runnable() {
      def run() {
        try runBinarySeachTreeCheck
        catch {
          case _ : Throwable =>
        }
      }
    }, "", 1 << 26).start()
  }
}
