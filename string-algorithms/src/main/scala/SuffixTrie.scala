import scala.io.StdIn

object SuffixTrie {
  def main(args: Array[String]): Unit = computeSuffixTrie(StdIn.readLine).foreach(println)

  object Node {
    def Root(sequence : Array[Char]) : Node = new Node(0, 0, sequence.length, Map.empty, None, sequence)
  }
  class Node(var start : Int, var end : Int, val level : Int, var children: Map[Char, Node], var parent: Option[Node], val sequence : Array[Char]) {
    def isRoot: Boolean = start == 0 && end == sequence.length
    def value : String = sequence.subSequence(start, end).toString
    def toSequence: Seq[String] = children.values.flatMap(desc => desc.value +: desc.toSequence)(collection.breakOut)
  }

  def computeSuffixTrie(input : String) : Seq[String] = {
    val sequence = input.toCharArray
    var leafs = Set.empty[Node]
    var nodes = Set.empty[Node]
    val root = Node.Root(sequence)

    for (offset <- sequence.indices) {
      var p : Node = root
      for (i <- offset until sequence.length) {
        val c = sequence.charAt(i)
        if (p.children.contains(c)) p = p.children(c)
        else {
          // Spawn
          val newNode = new Node(i, i + 1, p.level + 1, Map.empty, Some(p), sequence)
          p.children = p.children.updated(c, newNode)
          leafs = leafs - p + newNode
          p = p.children(c)
          nodes = nodes + newNode
        }
      }
    }
    compress(leafs, nodes.diff(leafs))
    root.toSequence
  }

  private def compress(leafs : Set[Node], nonLeafs : Set[Node]) : Unit = {
    var toProcess = nonLeafs
    def compress(node : Node) : Unit = {
      var n = node
      while (n.parent.isDefined && !n.parent.get.isRoot && n.parent.get.children.size == 1) {
        //println("Compressing node: " + n.parent.get.characters + s" with adding " + n.characters)
        n.parent.get.children = n.children
        n.parent.get.end = n.end
        //n.parent.get.computeValue()
        val oldChild = n
        n = n.parent.get
        oldChild.parent = None // prevent loitering
        oldChild.children = Map.empty
        toProcess = toProcess - n
      }
    }

    leafs.foreach(compress) // filter(_.children.isEmpty) not necessary due to invariant

    // Sorting to go bottom up in order to be deterministic and compliant to the tests
    toProcess.filter(_.children.size == 1).toSeq.sortBy(-_.level).map(_.children.head._2).foreach(compress)
  }
}
