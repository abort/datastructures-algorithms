import scala.io.StdIn

object Trie {
  def main(args: Array[String]): Unit = {
    val n : Int = StdIn.readInt
    var sequences : IndexedSeq[String] = IndexedSeq.fill(n)("")
    for (i <- 0 until n) {
      sequences = sequences.updated(i, StdIn.readLine())
    }

    computeTrie(sequences).foreach(println)
  }

  sealed trait Node {
    var children : Map[Char, CharNode]
    val id : Int

    def toSequence(): Seq[String] = {
      if (children.isEmpty) Seq.empty
      else {
        children.map { case (k,v) => s"$id->${v.id}:$k" }.toSeq ++ children.values.flatMap(_.toSequence()).toSeq
      }
    }
  }
  case class CharNode(character : Char, override var children: Map[Char, CharNode], override val id : Int) extends Node
  case class Root(override var children: Map[Char, CharNode]) extends Node { val id : Int = 0 }

  def computeTrie(sequences: IndexedSeq[String]) : Seq[String] = {
    val root = Root(Map.empty)
    var count = 1

    for (charSequence <- sequences.map(_.toCharArray)) {
      var p : Node = root
      for (c <- charSequence) {
        if (p.children.contains(c)) p = p.children(c)
        else {
          // Spawn
          p.children = p.children.updated(c, CharNode(c, Map.empty, count))
          count += 1
          p = p.children(c)
        }
      }
    }

    root.toSequence()
  }
}
