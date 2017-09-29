import scala.io.StdIn

object SuffixTrie {
  def main(args: Array[String]): Unit = {
    val input = StdIn.readLine
    computeSuffixTrie(input).foreach(println)
//    var output = Iterable.empty[String]
//    import org.scalameter._
//    val time = measure {
//      output = computeSuffixTrie(input)
//    }
//    //output.foreach(println)
//    println(s"Took $time")
  }

  type Offset = (Int, Int)
  object Node {
    def Root(sequence : Array[Char]) : Node = new Node(0, sequence.length, 0, Map.empty, null, sequence)
  }
  object Cache {
    def apply(): Cache = new Cache(-1, -1, "")
  }
  class Cache(var start : Int, var end : Int, var output : String)
  class Node(var start : Int, var end : Int, var level : Int, var children: Map[Char, Node], var parent: Node, val sequence : Array[Char]) extends Ordered[Node] {
    @inline def isChar : Boolean = end - start == 1

    override def compare(that: Node): Int = {
      if (start < that.start) start - that.start
      else if (start == that.start) end - that.end
      else start - that.start
    }

    def isRoot: Boolean = start == 0 && end == sequence.length
    def value(cache : Cache) : String = {
      if (start == cache.start) cache.output = cache.output + sequence.subSequence(cache.end, end).toString
      else if (isChar) cache.output = sequence.charAt(start).toString
      else cache.output = sequence.subSequence(start, end).toString

      cache.start = start
      cache.end = end
      cache.output
    }
    def length : Int = children.values.size + children.values.map(desc => desc.length).sum
    def toSequence(cache : Cache = Cache()): List[String] = children.values.toList.sorted.flatMap(desc => desc.value(cache) +: desc.toSequence(cache))(collection.breakOut)
    def isCompressionCandidate: Boolean = parent != null && parent.children.size == 1 && !parent.isRoot
  }

  def computeSuffixTrie(input : String) : Iterable[String] = {
    val sequence = input.toCharArray
    var leaves = Set.empty[Node]
    var nodes = Set.empty[Node]
    val root = Node.Root(sequence)

    for (offset <- sequence.indices) {
      var p : Node = root
      for (i <- offset until sequence.length) {
        val c = sequence(i)
        if (p.children.contains(c)) p = p.children(c)
        else {
          // Spawn lazily
          val node = new Node(i, i + 1, p.level + 1, Map.empty, p, sequence)
          p.children = p.children.updated(c, node)
          leaves = leaves - p + node
          p = p.children(c)
          nodes = nodes + node
        }
      }
    }

    val complement = compress(nodes, leaves).toList.sorted
    // assert(complement.length == root.length)

    // complement.map is faster than root.toSequence() most likely
    val cache = Cache()
    complement.map(_.value(cache))
  }

  private def compress(nodes : Set[Node], leaves : Set[Node]) : Set[Node] = {
    val nonLeaves = nodes.diff(leaves)
    var toProcess = nonLeaves
    var complement = nodes
    def compress(node : Node) : Unit = {
      var n = node
      while (n.isCompressionCandidate) {
        val p = n.parent
        p.children = n.children
        p.end = n.end

        toProcess = toProcess - n
        complement = complement - n
        // n gets deleted
        val c = n
        n = p
        c.parent = null // prevent loitering
        c.children = Map.empty
      }
    }

    // Bottom up first
    leaves.foreach(compress)

    // Sorting to go bottom up in order to be deterministic and compliant to the tests
    toProcess.filter(_.children.size == 1).toList.sortBy(-_.level).map(_.children.head._2).foreach(compress)

    // Left over nodes
    complement
  }
}