import scala.io.StdIn

object SuffixTrie {
  def main(args: Array[String]): Unit = {
    val input = StdIn.readLine
    var output = Iterable.empty[String]
    import org.scalameter._
    val time = measure {
      output = computeSuffixTrie(input)
    }
    output.foreach(println)
    println(s"Took $time to compute")
  }

  object Node {
    def Root(sequence : Array[Char]) : Node = new Node(0, sequence.length, Map.empty, null, sequence)
  }
  class Node(var start : Int, var end : Int, var children: Map[Char, Node], var parent: Node, val sequence : Array[Char]) {
    def isRoot: Boolean = start == 0 && end == sequence.length
    def value : String = sequence.subSequence(start, end).toString
    def asSequence: List[String] = children.values.flatMap(desc => desc.value +: desc.asSequence)(collection.breakOut)
  }

  def getCommonStringSkip(startPos : Int, existingStartPos : Int, existingEndPos : Int, sequence : Array[Char]) : Int = {
    if (startPos >= sequence.length || existingStartPos >= sequence.length) return 0

    var i = 0
    var stop = false
    while (!stop && sequence.charAt(startPos + i) == sequence.charAt(existingStartPos + i)) {
      i += 1
      stop = i >= sequence.length || (existingStartPos + i) >= sequence.length || (startPos + i) >= sequence.length || (existingStartPos + i) >= existingEndPos
    }

    i
  }

  def insertSequence(root : Node, offset : Int, sequence : Array[Char]) : List[Node] = {
    var addedNodes = List.empty[Node]
    var node = root
    var index = offset

    // Acquire the correct parent
    while (node.children.contains(sequence(index))) {
      val c = sequence(index)
      val subNode = node.children(c)
      val commonLen = getCommonStringSkip(index, subNode.start, subNode.end, sequence)
      val splitPoint = index + commonLen

      // We have reached the end of subnode, descent further
      if (subNode.end <= subNode.start + commonLen && splitPoint <= sequence.length - 1) {
        index = splitPoint
        node = subNode
      }
      else {
        // Split on common string and spawn new parent for it
        val key = sequence(subNode.start + commonLen)
        val newParent = new Node(index, splitPoint, Map(key -> subNode), null, sequence)
        addedNodes = addedNodes :+ newParent
        subNode.start = subNode.start + commonLen
        node.children = node.children.updated(c, newParent)
        index = splitPoint

        node = newParent
      }
    }
    // Spawn new child as it does not exist yet
    val newNode = new Node(index, sequence.length, Map.empty, null, sequence)
    addedNodes = addedNodes :+ newNode
    node.children = node.children.updated(sequence(index), newNode)

    addedNodes
  }

  def computeSuffixTrie(input : String) : Iterable[String] = {
    val sequence = input.toCharArray
    val root = Node.Root(sequence)

    var nodes = List.empty[Node]
    for (offset <- sequence.indices) {
      val addedNodes = insertSequence(root, offset, sequence)
      nodes = addedNodes ++ nodes
    }

    nodes.map(_.value)
  }
}