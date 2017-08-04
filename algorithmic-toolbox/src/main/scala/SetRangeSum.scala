import scala.io.StdIn

object SetRangeSum {
  case class Node(var key : Int, var left : Option[Node], var right : Option[Node], var parent : Option[Node]) {
    def hasNoChildren : Boolean = left.isEmpty && right.isEmpty
    def hasOneChild : Boolean = (left.isDefined && right.isEmpty) || (left.isEmpty && right.isDefined)
    def hasParent : Boolean = parent.nonEmpty
  }

  def runSetRangeSum() : Unit = {
    import BinaryTree._ // TODO: optimize with splaytree

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

          x = sum(root, (line(1).toInt + x) % m, (line(2).toInt + x) % m)
          println(x)
        }
      }
    }
  }

  object SplayTree {
    def find(root : Option[Node], n : Int): Option[Node] = {
      val node = BinaryTree.find(root, n)
      BinaryTree.splay(root, node)
      node
    }

    def add(root : Option[Node], n : Int) : Node = {
      val result = BinaryTree.add(root, n)
      BinaryTree.splay(root, Some(result)).get
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
      root
    }
    def splay(root : Option[Node], node: Option[Node]) : Option[Node] = {
      root
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
      var total = 0
      for (i <- left to right) {
        total = total + find(root, i).map(_.key).getOrElse(0)
      }
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
