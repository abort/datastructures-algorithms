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
    var myValue = value(Cache())

    def isRoot: Boolean = start == 0 && end == sequence.length
    def value(cache : Cache) : String = {
      if (start == cache.start) cache.output = cache.output + sequence.subSequence(cache.end, end).toString
      else if (isChar) cache.output = sequence.charAt(start).toString
      else cache.output = sequence.subSequence(start, end).toString

      cache.start = start
      cache.end = end
      cache.output
    }
    def update() : Unit = {
      myValue = value(Cache())
      children.values.foreach(_.update())
    }
    def length : Int = children.values.size + children.values.map(desc => desc.length).sum
    def get() : List[String] = children.values.flatMap(desc => desc.value(Cache()) +: desc.get()).filter(_.nonEmpty).toList
    def toSequence(cache : Cache = Cache()): List[String] = children.values.toList.sorted.flatMap(desc => desc.value(cache) +: desc.toSequence(cache))(collection.breakOut)
    def isCompressionCandidate: Boolean = parent != null && parent.children.size == 1 && !parent.isRoot
  }

  def getCommonStringSkip(startPos : Int, existingStartPos : Int, existingEndPos : Int, sequence : Array[Char]) : Int = {
    if (startPos >= sequence.length || existingStartPos >= sequence.length) return 0

    var i = 0
    //var j = existingStartPos
    var stop = false
    while (!stop && sequence.charAt(startPos + i) == sequence.charAt(existingStartPos + i)) {
      i += 1
      stop = i >= sequence.length || (existingStartPos + i) >= sequence.length || (startPos + i) >= sequence.length || (existingStartPos + i) >= existingEndPos
    }

   // println(s"equal characters for ${sequence.mkString.substring(startPos)} and ${sequence.mkString.substring(existingStartPos)} = $i")

    i
  }

  def updateTree(root : Node, offset : Int, sequence : Array[Char]) : Unit = {
    if (offset > sequence.length) return
    //println(s"Adding: ${sequence.subSequence(offset, sequence.length)}")

    var node = root
    var index = offset
    while (node != null && index < sequence.length) {
      val c = sequence(index)
      if (node.children.contains(c)) {
        val subNode = node.children(c)
        val commonLen = getCommonStringSkip(index, subNode.start, subNode.end, sequence)
        //if (index + commonLen < sequence.length) println(s"First different char: ${sequence(index + commonLen)} in ${sequence.mkString.substring(index + commonLen)}")
        if (index + commonLen == sequence.length) { // 100% equality
          // TODO: increase repeats?
          return
        }
        val splitPoint = index + commonLen

        // there is a divergence...
        // We have reached the end of subnode, descent further

        if (subNode.end <= subNode.start + commonLen && splitPoint <= sequence.length - 1) {
          //println(s"descending to ${sequence.subSequence(subNode.start, subNode.end)} with ${sequence(splitPoint - 1)}")
          index = splitPoint
          node = subNode
        }
        else {
          // Split on common string and spawn new parent for it
          //println(s"splitting ${sequence.subSequence(subNode.start, subNode.end)} to ${sequence.subSequence(index, splitPoint)}")
          val key = sequence(subNode.start + commonLen)
          val newParent = new Node(index, splitPoint, 0, Map(key -> subNode), null, sequence)
          subNode.start = subNode.start + commonLen
          node.children = node.children.updated(c, newParent)
          index = splitPoint
          //println(s"new parent: " + newParent.value(Cache()) + "\n\tchildren: " + newParent.children.map { case (k, v) => s"\t\t$k -> ${v.value(Cache())}" }.mkString("\n", "\n", "\n"))

          //println(s"after split descent to ${sequence.subSequence(newParent.start, newParent.end)} with ${sequence.mkString.substring(splitPoint)}")
          node = newParent
        }
      }
      else {
        //println(s"parent (root: ${node.isRoot}) has no $c, creating new child ${sequence.subSequence(index, sequence.length)}")
        val newNode = new Node(index, sequence.length, 0, Map.empty, null, sequence)
        node.children = node.children.updated(c, newNode)
        //println(s"parent: " + node.children.map { case (k, v) => s"$k -> ${v.value(Cache())}" }.mkString("\n", "\n", "\n"))
        node = null
      }
    }
    println()
  }

  def computeSuffixTrie(input : String) : Iterable[String] = {
    val sequence = input.toCharArray
    var leaves = Set.empty[Node]
    var nodes = Set.empty[Node]
    val root = Node.Root(sequence)

    for (offset <- sequence.indices) {
      var p : Node = root
      updateTree(root, offset, sequence)
    }

    /*
    val complement = compress(nodes, leaves).toList.sorted
    // assert(complement.length == root.length)

    // complement.map is faster than root.toSequence() most likely
    val cache = Cache()
    complement.map(_.value(cache))
    */
    root.update()
    root.get()
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