import scala.io.StdIn

object SuffixTrie {
  def main(args: Array[String]): Unit = computeSuffixTrie(StdIn.readLine).foreach(println)

  object Node {
    def Root(sequence : Array[Char]) : Node = new Node(0, 0, sequence.length, Map.empty, null, sequence)
  }
  class Node(var start : Int, var end : Int, val level : Int, var children: Map[Char, Node], var parent: Node, val sequence : Array[Char]) {
    def isRoot: Boolean = start == 0 && end == sequence.length
    def value : String = sequence.subSequence(start, end).toString
    def toSequence: Seq[String] = children.values.flatMap(desc => desc.value +: desc.toSequence)(collection.breakOut)
    def isOnlyChild: Boolean = parent != null && parent.children.size == 1
  }

  def computeSuffixTrie(input : String) : Iterable[String] = {
    val sequence = input.toCharArray
    var leaves = Set.empty[Node]
    var nodes = Set.empty[Node]
    val root = Node.Root(sequence)

    for (offset <- sequence.indices) {
      var p : Node = root
      for (i <- offset until sequence.length) {
        val c = sequence.charAt(i)
        if (p.children.contains(c)) p = p.children(c)
        else {
          // Spawn
          val newNode = new Node(i, i + 1, p.level + 1, Map.empty, p, sequence)
          p.children = p.children.updated(c, newNode)
          leaves = leaves - p + newNode
          p = p.children(c)
          nodes = nodes + newNode
        }
      }
    }
    val complement = compress(nodes, leaves)
    // complement.map is faster than toSequence most likely
    complement.map(_.value)
  }

  private def compress(nodes : Set[Node], leaves : Set[Node]) : Set[Node] = {
    val nonLeaves = nodes.diff(leaves)
    var toProcess = nonLeaves
    var complement = nodes
    def compress(node : Node) : Unit = {
      var n = node
      while (n.parent != null && !n.parent.isRoot && n.isOnlyChild) {
        val p = n.parent
        p.children = n.children
        p.end = n.end

        // n gets deleted
        val c = n
        toProcess = toProcess - n
        complement = complement - n
        n = p
        c.parent = null // prevent loitering
        c.children = Map.empty
      }
    }
    leaves.foreach(compress)

    // Sorting to go bottom up in order to be deterministic and compliant to the tests
    toProcess.filter(_.children.size == 1).toSeq.sortBy(-_.level).map(_.children.head._2).foreach(compress)

    // Left over nodes
    complement
  }
}