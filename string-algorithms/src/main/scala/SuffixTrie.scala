import scala.collection.mutable
import scala.io.StdIn

object SuffixTrie {
  def main(args: Array[String]): Unit = {
    val input = StdIn.readLine
    var output = Iterable.empty[String]
    import org.scalameter._
    val time = measure {
      output = computeSuffixTrie(input)
    }
    //output.foreach(println)
    println(s"Took $time")
  }

  type Offset = (Int, Int)
  object Node {
    def Root(sequence : Array[Char]) : Node = new Node((0, sequence.length), 0, Map.empty, null, sequence)
  }
  // Keep level as low as possible!
  class Node(private var offset : Offset, var level : Int, var children: Map[Char, Node], var parent: Node, val sequence : Array[Char]) extends Ordered[Node] {
    var start : Int = offset._1
    var end : Int = offset._2

    override def compare(that: Node): Int = {
      if (start < that.start) start.compareTo(that.start)
      else if (start == that.start) end.compareTo(that.end)
      else that.start.compareTo(start)
    }

    def isRoot: Boolean = start == 0 && end == sequence.length
    def value(cache : mutable.Map[Offset, String]) : String = {
      if (cache.contains((start, end))) {
        // println("Cached!!!")
        cache((start, end))
      }
      else if (cache.contains((start, end - 1))) {
        val result = cache((start, end - 1)) + sequence(end)
        cache.put((start, end), result)
        result
      }
      else {
        val result = sequence.subSequence(start, end).toString
        cache.put((start, end), result)
        result
      }
    }
    def toSequence(cache : mutable.Map[Offset, String] = mutable.Map.empty): Seq[String] = children.values.toSeq.sorted.flatMap(desc => desc.value(cache) +: desc.toSequence(cache))(collection.breakOut)
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
          val offset = (i, i + 1)
          val newLevel = p.level + 1
          // Spawn lazily
          val node = new Node(offset, newLevel, Map.empty, p, sequence)
          p.children = p.children.updated(c, node)
          leaves = leaves - p + node
          p = p.children(c)
          nodes = nodes + node
        }
      }
    }
    val complement = compress(nodes, leaves)
    // complement.map is faster than toSequence most likely
    root.toSequence(mutable.Map.empty[Offset, String])

    // complement.toSeq.sorted
//    val cache = mutable.Map.empty[Offset, String]
//    complement.toSeq.sorted.map(_.value(cache))
  }

  private def compress(nodes : Set[Node], leaves : Set[Node]) : Set[Node] = {
    val nonLeaves = nodes.diff(leaves)
    var toProcess = nonLeaves
    var complement = nodes
    @inline def compress(node : Node) : Unit = {
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