import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn

object NonSharedSubstring {
  def main(args: Array[String]): Unit = {
    val sequence = StdIn.readLine
    val comparisonSequence = StdIn.readLine
    val output = computeShortestNonSharedSubString(sequence, comparisonSequence)
    if (output.isEmpty) println("Provided sequences are the same")
    else println(output.get)
/*
    var output : Option[String] = None
    import org.scalameter._
    val time = measure {
      output = computeShortestNonSharedSubString(sequence, comparisonSequence)
    }
    if (output.isEmpty) println("Provided sequences are the same")
    else println(output.get)
    println(s"Took $time to compute")
  */
  }

  object Node {
    def Root(sequence : Array[Char]) : Node = new Node(0, 0, Map.empty, null, sequence)
  }
  class Node(var start : Int, var end : Int, var children: Map[Char, Node], var parent: Node, val sequence : Array[Char]) {
    lazy val length = end - start
    def isRoot: Boolean = start == 0 && end == 0
    lazy val value : String = sequence.subSequence(start, end).toString
    @tailrec
    final def contains(input : String, accumulated : String = "") : Boolean = {
//      println(s"in $value node")
      if (isRoot) {
//        println(s"checking for ${input.head}")
        if (children.contains(input.head)) children(input.head).contains(input, value)
        else false
      }
      else if (length >= input.length && value.substring(0, input.length) == input) true
      else if (input.take(length) == value) {
//        println(s"value: $value")
        // dispatch rest to child
        val remainder = input.drop(length)
//        println(s"Remainder: $remainder")
        if (remainder.isEmpty) true
        else if (children.contains(remainder.head)) children(remainder.head).contains(remainder, value)
        else false
      }
      else {
//        println("Else")
        false
      }
    }
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

  def computeShortestNonSharedSubString(sequence : String, comparisonSequence : String) : Option[String] = {
    val root = computeSuffixTrie(comparisonSequence)
    allSubstrings(sequence).find(!root.contains(_))
  }

  def allSubstrings(input : String) : Seq[String] = {
    val ordering : Ordering[String] = Ordering.by { s => s.length() }
    val queue = mutable.PriorityQueue.empty[String](ordering.reverse)
    var processed = Set.empty[String]
    for (i <- input.indices) {
      for (j <- i until input.length) {
        val seq = input.substring(i, j + 1)
        if (!processed.contains(seq)) {
          processed = processed + seq
          queue.enqueue(seq)
        }
      }
    }
    var output = Seq.empty[String]
    while (queue.nonEmpty) {
      output = output :+ queue.dequeue()
    }
    output
  }

  def computeSuffixTrie(input : String) : Node = {
    val augmented = input + '$'
    val sequence = augmented.toCharArray
    val root = Node.Root(sequence)

    var nodes = List.empty[Node]
    for (offset <- sequence.indices) {
      val addedNodes = insertSequence(root, offset, sequence)
      nodes = addedNodes ++ nodes
    }

    root
  }
}