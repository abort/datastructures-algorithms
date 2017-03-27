import scala.collection.mutable
import scala.io.StdIn

object TreeHeight {
  def main(args: Array[String]): Unit = {
    val nodeCount = StdIn.readInt
    val parentsInput = StdIn.readLine.split(' ').map(_.toInt)
    // Start with root node
    val sorted = parentsInput.zipWithIndex
    val parents = Array.ofDim[Int](nodeCount)
    val heights = Array.fill[Int](nodeCount)(0)

    for ((p, i) <- sorted) {
      parents(i) = p
    }

    def getMaxHeight(): Int = {
      var maxHeight = 0
      val stack = mutable.Stack[Int]()
      for (index <- 0 until nodeCount) {

        if (heights(index) == 0) {
          stack.push(index)
          // Can be optimized by counting height (size of stack) and then unrolling (like in the previous code)
          while (stack.nonEmpty) {
            val head = stack.head

            val parent = parents(head)
            if (parent == -1) {
              heights(head) = 1
              stack.pop
            }
            else if (heights(parent) == 0) {
              stack.push(parent)
            }
            else if (heights(parent) != 0) {
              heights(stack.pop) = heights(parent) + 1
            }
          }

          maxHeight = Math.max(maxHeight, heights(index))
        }
      }

      maxHeight
    }

    println(getMaxHeight)
  }
}
