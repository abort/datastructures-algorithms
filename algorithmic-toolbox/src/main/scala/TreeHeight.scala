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
      for (index <- 0 until nodeCount) {
        if (heights(index) == 0) {
          var height = 0
          var node = index
          var run = true
          while (run && node != -1) {
            if (heights(node) != 0) {
              height += heights(node)
              run = false
            }
            else {
              node = parents(node)
              height += 1
            }
          }

          maxHeight = Math.max(maxHeight, height)
          run = true
          node = index
          while (run && node != -1) {
            if (heights(node) != 0) run = false
            else {
              heights(node) = height
              height -= 1
              node = parents(node)
            }
          }
        }
      }

      maxHeight
    }

    def computeHeight(i: Int): Int = {
      if (heights(i) != 0) return heights(i)

      val stack = mutable.Stack[Int](i)
      while (stack.nonEmpty) {
        val popped = stack.pop
        println(s"pop: ${popped}")
        val parent = parents(popped)
        if (parent == -1) {
          heights(popped) = 1
        }
        else if (heights(parent) == 0) stack.push(parent)
        else heights(popped) = heights(parent) + 1
      }

      heights(i)
    }

    println(getMaxHeight)
  }
}
