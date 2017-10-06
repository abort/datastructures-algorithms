import scala.annotation.tailrec
import scala.io.StdIn

object NonSharedSubstring {
  def main(args: Array[String]): Unit = {
    val sequence = StdIn.readLine
    val comparisonSequence = StdIn.readLine

    val output = computeShortestNonSharedSubString(sequence, comparisonSequence)
    output.foreach(println)
  }


  object Node {
    def Root(sequence : Array[Char]) : Node = new Node(0, 0, Map.empty, null, sequence)
  }
  class Node(var start : Int, var end : Int, var children: Map[Char, Node], var parent: Node, val sequence : Array[Char]) {
    lazy val isRoot: Boolean = start == 0 && end == 0
    lazy val length : Int = end - start
    lazy val value : String = sequence.subSequence(start, end).toString

    @tailrec
    final def traverse(s : String, accumulated : String = "") : Option[String] = {
      if (s.isEmpty) None
      else if (isRoot) {
        if (children.contains(s.head)) children(s.head).traverse(s, accumulated)
        else Some(s.head.toString)
      }
      else {
        val consumed = value.zipWithIndex.takeWhile { case (c, i) => i < s.length && c == s(i) }.map(_._1).mkString
        if (s.length == consumed.length) None
        else if (s.length > value.length) {
          if (consumed.length == length && children.contains(s(consumed.length))) {
            children(s(consumed.length)).traverse(s.substring(consumed.length), accumulated + value)
          }
          else {
            Some(accumulated + consumed + s(consumed.length))
          }
        }
        else if (s.length < length) Some(accumulated + consumed + s(consumed.length))
        else if (consumed.length < length) Some(accumulated + consumed + s(consumed.length))
        else None
      }
    }
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

  def insertSequence(root : Node, offset : Int, sequence : Array[Char]) : Unit = {
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
        subNode.start = subNode.start + commonLen
        node.children = node.children.updated(c, newParent)
        index = splitPoint

        node = newParent
      }
    }
    // Spawn new child as it does not exist yet
    val newNode = new Node(index, sequence.length, Map.empty, null, sequence)
    node.children = node.children.updated(sequence(index), newNode)
  }

  def computeShortestNonSharedSubString(sequence : String, comparisonSequence : String) : Option[String] = {
    val root = computeSuffixTrie(comparisonSequence)
    var output = Option.empty[String]
    var minLength = Int.MaxValue
    for (i <- sequence.indices) {
      val result = root.traverse(sequence.substring(i))
      if (result.exists(_.length < minLength)) {
        output = result
        minLength = output.get.length
        if (minLength == 1) return result // we won't find a smaller example than this for sure :)
      }
    }
    output
  }

  def computeSuffixTrie(input : String) : Node = {
    val sequence = (input + '$').toCharArray
    val root = Node.Root(sequence)

    sequence.indices.foreach(insertSequence(root, _, sequence))

    root
  }
}