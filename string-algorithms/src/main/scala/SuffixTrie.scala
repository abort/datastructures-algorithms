import scala.io.StdIn

object SuffixTrie {
  def main(args: Array[String]): Unit = {
    computeSuffixTrie(StdIn.readLine).foreach(println)
  }

  sealed trait Node {
    var parent : Option[Node]
    var children : Map[String, CharNode]
    val id : Int
    var characters : String

    def toSequence: Seq[String] = {
      if (children.isEmpty) Seq.empty
      else {
        // s"$id->${v.id}:${v.characters}"
        //children.map { case (k,v) => s"$id->${v.id}:${v.characters}" }.toSeq ++ children.values.flatMap(_.toSequence).toSeq
        children.values.flatMap(desc => desc.characters +: desc.toSequence)(collection.breakOut)
      }
    }
  }
  class CharNode(override var characters: String, override var children: Map[String, CharNode], override val id : Int, override var parent: Option[Node]) extends Node
  class Root(override var children: Map[String, CharNode]) extends Node {
    override val id : Int = 0
    override var parent : Option[Node] = None
    override var characters : String = ""
  }

  def computeSuffixTrie(input : String) : Seq[String] = computeSuffixTrie(input.indices.map(i => input.substring(i)))

  private def computeSuffixTrie(sequences : Seq[String]) = {
    println("inputs: " + sequences.mkString("[", ", ", "]"))
    var leafs = Set.empty[Node]
    val root = new Root(Map.empty)
    var count = 1

    for (charSequence <- sequences.map(_.toCharArray)) {
      var p : Node = root
      for (c <- charSequence) {
        val asString = c.toString
        if (p.children.contains(asString)) p = p.children(asString)
        else {
          // Spawn
          val newNode = new CharNode(asString, Map.empty, count, Some(p))
          p.children = p.children.updated(asString, newNode)
          leafs = leafs - p + newNode
          count += 1
          p = p.children(asString)
        }
      }
    }
    leafs.filter(_.children.isEmpty).foreach(compress)
    root.toSequence
  }

  def compress(node : Node) : Node = {
    if (node.children.isEmpty) {
      var n = node
      while (n.parent.isDefined && n.parent.get.children.size == 1) {
        //println("Compressing node: " + n.parent.get.characters + s" with adding " + n.characters)
        n.parent.get.children = Map.empty
        println(s"\tSet ${n.parent.get.characters} to ${n.parent.get.characters + n.characters}")
        n.parent.get.characters = n.parent.get.characters + n.characters
        n = n.parent.get
      }
    }
    node
  }
}
