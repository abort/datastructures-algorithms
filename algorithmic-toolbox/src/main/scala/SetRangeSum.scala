import scala.io.StdIn

object SetRangeSum {
  case class Node(var key : Int, var left : Option[Node], var right : Option[Node], var parent : Option[Node]) {
    def hasNoChildren : Boolean = left.isEmpty && right.isEmpty
    def hasOneChild : Boolean = (left.isDefined && right.isEmpty) || (left.isEmpty && right.isDefined)
    def hasParent : Boolean = parent.nonEmpty

    def inOrder(f: Int => Boolean) : Unit = {
      left.map(_.inOrder(f))
      if (f(key)) return
      right.map(_.inOrder(f))
    }
  }


  def runSetRangeSum() : Unit = {
    //import SplayTree._ // TODO: optimize with splaytree
    import BinaryTree._

    val m = 1000000001
    var x = 0
    val operations = StdIn.readInt
    var root = Option.empty[Node]
    for (_ <- 0 until operations) {
      val line = StdIn.readLine.trim.split(' ')


      val op = line(0)
      //(line(0), line(1).toInt)
      op match {
        case "?" => {
          println(if (find(root, (line(1).toInt + x) % m).isDefined) "Found" else "Not found")
        }
        case "+" => {
          root = Some(add(root, (line(1).toInt + x) % m))
        }
        case "-" => {
          root = del(root, (line(1).toInt + x) % m)
        }
        case "s" => {

          x = BinaryTree.sum(root, (line(1).toInt + x) % m, (line(2).toInt + x) % m)
          println(x)
        }
      }
    }
  }

  object SplayTree {
    def find(root : Option[Node], n : Int): Option[Node] = {
      val node = BinaryTree.find(root, n)
      BinaryTree.splay(root, n)
      node
    }

    def add(root : Option[Node], n : Int) : Node = {
      val result = BinaryTree.add(root, n)
      BinaryTree.splay(Some(result), n).get
    }

    def del(root : Option[Node], n : Int) : Option[Node] = {
      if (root.isEmpty) root
      else {
        var node = BinaryTree.splay(root, n)

        if (node.exists(_.key == n)) {
          if (node.get.left.isEmpty) node = node.get.right
          else {
            val x = node.get.right
            node = node.get.left
            node = BinaryTree.splay(node, n)
            node.get.right = x
          }
        }
        node
      }
    }
  }
  object BinaryTree {
    def splay(root : Option[Node], n : Int) : Option[Node] = {
      if (root.isEmpty) return root

      var node = root.get
      val key = node.key
      if (n < key) {
        if (node.left.isEmpty) return root

        val leftNode = node.left.get
        val leftKey = leftNode.key
        if (n < leftKey) {
          leftNode.left = splay(leftNode.left, n)
          node = rotateRight(node)
        }
        else if (n > leftKey) {
          leftNode.right = splay(leftNode.right, n)
          if (leftNode.right.isDefined)
            node.left = Some(rotateLeft(node))
        }

        if (node.left.isEmpty) return Some(node)
        else return Some(rotateRight(node))
      }
      else if (n > key) {
        if (node.right.isEmpty) return root

        val rightNode = node.right.get
        val rightKey = rightNode.key
        if (n < rightKey) {
          rightNode.left = splay(rightNode.left, n)
          if (rightNode.left.isDefined)
            node.right = Some(rotateRight(node.right.get))
        }
        else if (n > rightKey) {
          rightNode.right = splay(rightNode.right, n)
          node = rotateLeft(node)
        }

        if (node.right.isEmpty) return Some(node)
        else return Some(rotateLeft(node))
      }
      else root
    }

    private def rotateRight(n : Node) : Node = {
      val x = n.right.get
      n.right = x.left
      x.left = Some(n)
      x
    }

    private def rotateLeft(n : Node) : Node = {
      val x = n.right.get
      n.right = x.left
      x.left = Some(n)
      x
    }

    def add(root : Option[Node], n : Int) : Node = {
      val result = Node(n, None, None, None)
      if (root.isEmpty) {
        return result
      }

      val node = root.get
      if (n == node.key) return node
      else if (n < node.key) {
        if (node.left.isDefined) node.left = Some(add(node.left, n))
        else {
          result.parent = root
          node.left = Some(result)
        }
      }
      else if (n > node.key) {
        if (node.right.isDefined) node.right = Some(add(node.right, n))
        else {
          result.parent = root
          node.right = Some(result)
        }
      }
      node
    }

    def del(root : Option[Node], n : Int) : Option[Node] = {
      if (root.isEmpty) return None
      val node = root.get

      if (node.key == n) {
        if (node.left.isEmpty) return node.right
        if (node.right.isEmpty) return node.left

        // find minimum
        node.key = min(node.right)
        node.right = del(node.right, node.key)
      }
      else if (n < node.key) node.left = del(node.left, n)
      else if (n > node.key) node.right = del(node.right, n)

      Some(node)
    }

    def find(root : Option[Node], n : Int) : Option[Node] = {
      if (root.isEmpty) None
      else if (root.get.key == n) root
      else if (n < root.get.key) find(root.get.left, n)
      else if (n > root.get.key) find(root.get.right, n)
      else None
    }

    def sum(root : Option[Node], left : Int, right : Int) : Int = {
      if (root.isEmpty) return 0
      var total = 0
      def f(x : Int) : Boolean = {
        if (x >= left && x <= right) {
          total += x
        }

        x > right
      }
      //      for (i <- left to right) {
//        total = total + find(root, i).map(_.key).getOrElse(0)
//      }

      root.get.inOrder(f)
      total
    }

    def min(root : Option[Node]) : Int = {
      if (root.isEmpty) throw new IllegalStateException
      if (root.get.left.isEmpty) root.get.key
      else min(root.get.left)
    }
  }

  def main(args: Array[String]): Unit = {
    new Thread(null, new Runnable() {
      def run() {
        try runSetRangeSum()
        catch {
          case _ : Throwable =>
        }
      }
    }, "", 1 << 26).start()
  }
}
