import scala.annotation.tailrec
import scala.io.StdIn

object TrieMatching {
  def main(args: Array[String]): Unit = {
    val text = StdIn.readLine
    val n : Int = StdIn.readInt
    val sequences : Array[String] = Array.ofDim[String](n)
    var minLength : Int = Int.MaxValue
    for (i <- 0 until n) {
      val input = StdIn.readLine
      sequences(i) = input
      if (input.length < minLength) minLength = input.length
    }

    computeOccurrences(text, sequences, minLength)
  }


  def index(letter: Char): Int =  {
    if (letter == 'A') 0
    else if (letter == 'C') 1
    else if (letter == 'G') 2
    else if (letter == 'T') 3
    else -1
  }
  sealed trait Node {
    var children : Array[CharNode]
    var pattern = false
  }
  @inline private val radix = 4 // A up to T
  class CharNode(val char: Char, override var children: Array[CharNode]) extends Node {
    @tailrec
    final def occurs(needle : Array[Char]) : Boolean = {
      if (pattern) true // Reached bottom node!
      else if (needle.nonEmpty && children(index(needle.head)) != null) children(index(needle.head)).occurs(needle.tail)
      else false
    }
  }
  class Root(override var children: Array[CharNode]) extends Node {
    // Occurs is separated make tail recursion possible
    def occurs(needle : Array[Char]) : Boolean = {
      if (children(index(needle.head)) == null) false
      else children(index(needle.head)).occurs(needle.tail)
    }
  }

  @inline private def newCharArray() : Array[CharNode] = Array.ofDim[CharNode](radix)

  def computeOccurrences(text : String, patterns: Array[String], minPatternLength : Int) : Unit = {
    val root : Root = new Root(newCharArray())

    for (charSequence <- patterns.map(_.zipWithIndex)) {
      var p : Node = root
      for ((c, offset) <- charSequence) {
        val i = index(c)
        if (p.children(i) != null) p = p.children(i)
        else {
          // Spawn
          p.children(i) = new CharNode(c, newCharArray())
          p = p.children(i)
        }

        if (offset == charSequence.length - 1) p.pattern = true
      }
    }

    // TODO: sort by length (long to short) in case of unequal length patterns??
    val upperbound = text.length - minPatternLength
    val output = new java.lang.StringBuilder() //Seq.empty[Int]
    val textArray = text.toCharArray
    for (i <- 0 to upperbound) {
      // println(s"Finding subsequence ($i): ${text.substring(i)}")
      var node : Node = root
      var j = i
      var exhausted = false
      while (!exhausted && j < text.length) {
        val c = textArray(j)
        // println(s"\tmatching $c against trie")
        val child = node.children(index(c))
        if (child != null) {
          node = child
          if (node.pattern) {
            output.append(i)
            output.append(' ')
            exhausted = true
          }
          j = j + 1
        }
        else {
          exhausted = true
        }
      }
    }

    println(output.toString)
  }
}
