import scala.io.StdIn

object SuffixTrie {
  def main(args: Array[String]): Unit = {
    val input = StdIn.readLine
    computeSuffixTrie(input).foreach(println)
    /*
    var output = Iterable.empty[String]
    import org.scalameter._
    val time = measure {
      output = computeSuffixTrie(input)
    }
    //output.foreach(println)
    println(s"Took $time")
    */
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
    //var j = existingStartPos
    var stop = false
    while (!stop && sequence.charAt(startPos + i) == sequence.charAt(existingStartPos + i)) {
      i += 1
      stop = i >= sequence.length || (existingStartPos + i) >= sequence.length || (startPos + i) >= sequence.length || (existingStartPos + i) >= existingEndPos
    }

   // println(s"equal characters for ${sequence.mkString.substring(startPos)} and ${sequence.mkString.substring(existingStartPos)} = $i")

    i
  }

  def updateTree(root : Node, offset : Int, sequence : Array[Char]) : List[Node] = {
//    if (offset > sequence.length) return
    //println(s"Adding: ${sequence.subSequence(offset, sequence.length)}")

    var addedNodes = List.empty[Node]
    var node = root
    var index = offset
    while (node != null && index < sequence.length) {
      val c = sequence(index)
      if (node.children.contains(c)) {
        val subNode = node.children(c)
        val commonLen = getCommonStringSkip(index, subNode.start, subNode.end, sequence)
        val splitPoint = index + commonLen


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
          val newParent = new Node(index, splitPoint, Map(key -> subNode), null, sequence)
          addedNodes = addedNodes :+ newParent
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
        val newNode = new Node(index, sequence.length, Map.empty, null, sequence)
        addedNodes = addedNodes :+ newNode
        node.children = node.children.updated(c, newNode)
        //println(s"parent: " + node.children.map { case (k, v) => s"$k -> ${v.value(Cache())}" }.mkString("\n", "\n", "\n"))
        node = null
      }
    }
    addedNodes
  }

  def computeSuffixTrie(input : String) : Iterable[String] = {
    val sequence = input.toCharArray
    val root = Node.Root(sequence)

    var nodes = List.empty[Node]
    for (offset <- sequence.indices) {
      val addedNodes = updateTree(root, offset, sequence)
      nodes = addedNodes ++ nodes
    }

    nodes.map(_.value)
    // Too much recursion, hence we dont do it the beautiful way: root.asSequence
  }
}