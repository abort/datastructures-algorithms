import scala.io.StdIn

object DisjointSetQuestions {
  abstract class DisjointSet(n : Int) {
    def getType: this.type = this
    println(s"Instantiated ${getType.getClass().getSimpleName()} with $n disjoint sets")


    def makeSet(i : Int) : Unit
    def find(i : Int) : Int
    def union(i : Int, j : Int) : Unit
    def print() : Unit
  }

  class NaiveDisjointSet(n : Int) extends DisjointSet(n) {
    val smallest : Array[Int] = Array.ofDim[Int](n + 1)

    for (i <- smallest.indices.tail) makeSet(i)

    override def makeSet(i: Int): Unit = smallest(i) = i
    override def find(i: Int): Int = smallest(i)
    override def union(i: Int, j: Int): Unit = {
      val iRoot = find(i)
      val jRoot = find(j)
      if (iRoot == jRoot) return

      val m = Math.min(iRoot, jRoot)
      for (k <- smallest.indices) {
        if (smallest(k) == iRoot || smallest(k) == jRoot)
          smallest(k) = m
      }
    }

    override def print() = {
      println("indices:\t" + smallest.indices.tail.mkString(" "))
      println("values:\t" + smallest.tail.mkString(" "))
    }
  }

  class TreeDisjointSet(n : Int) extends DisjointSet(n) {
    val parent : Array[Int] = Array.ofDim[Int](n + 1)
    val rank : Array[Int] = Array.ofDim[Int](n + 1) // stores height of subtree rooted at i
    var pathCompression : Boolean = false

    for (i <- parent.indices.tail) makeSet(i)

    override def makeSet(i: Int): Unit = {
      parent(i) = i
      rank(i) = 0
    }
    override def find(i: Int): Int = {
      if (pathCompression) {
        if (i != parent(i)) {
          parent(i) = find(parent(i))
        }
        parent(i)
      }
      else {
        var k = i
        while (k != parent(k)) {
          k = parent(k)
        }
        k
      }
    }
    override def union(i: Int, j: Int): Unit = {
      val iRoot = find(i)
      val jRoot = find(j)
      if (iRoot == jRoot) return
      if (rank(iRoot) > rank(jRoot)) {
        parent(jRoot) = iRoot
      }
      else {
        parent(iRoot) = jRoot
        if (rank(iRoot) == rank(jRoot)) {
          rank(jRoot) += 1
        }
      }
    }

    override def print() = {
      println("path compression:\t" + pathCompression)
      println("indices:\t" + parent.indices.tail.mkString(" "))
      println("values:\t\t" + parent.tail.mkString(" "))
      println("ranks:\t\t" + rank.tail.mkString(" "))
      println("multiplicated ranks:\t" + rank.tail.product)
      println("max height:\t\t\t" + rank.tail.max)
    }
  }

  def startDisjointSetShell(line : Array[String]) = {
    assert(line.size >= 2)
    val m = line(0)
    val n = line(1).toInt
    val c = m match {
      case "n" => new NaiveDisjointSet(n)
      case "t" => new TreeDisjointSet(n)
      case "p" => {
        val t = new TreeDisjointSet(n)
        t.pathCompression = true
        t
      }
    }

    var running = true
    while (running) {
      val lineString = StdIn.readLine()
      val line = lineString.split(" ")
      val cmd = line(0)
      val params = line.tail
      if (cmd.startsWith("Union(")) {
        val params = lineString.replace("Union(", "").dropRight(1).split(",")
        println(s"Union of ${params.mkString(" and ")}")
        c.union(params(0).trim.toInt, params(1).trim.toInt)
      }
      else if ("u".equals(cmd)) {
        println(s"Union of ${params.mkString(" and ")}")
        c.union(params(0).toInt, params(1).toInt)
      }
      else if ("f".equals(cmd)) {
        val found = c.find(params(0).toInt)
        println(s"Found ${found}")
      }
      else if ("p".equals(cmd)) {
        c.print()
      }
      else if ("r".equals(cmd)) {
        running = false
        println("Restarting")
      }
      else if ("lui".equals(cmd)) {
        // Loop union
        println(s"Loop union increment")
        if (params.length >= 1) {
          for (i <- 1 to params(0).toInt) {
            println(s"\tUnion of ${i} and ${i + 1}")
            c.union(i, i + 1)
          }
        }
      }
      else if ("lu".equals(cmd)) {
        // Loop union
        println(s"Loop union ${params.mkString(" ")}")
        if (params.length >= 2) {
          for (i <- 1 to params(0).toInt) {
            val element = params(1).toInt * i
            println(s"\tUnion of ${i} and ${element}")
            c.union(i, element)
          }
        }
      }
      else if ("lf".equals(cmd)) {
        println(s"Loop find ${params.mkString(" ")}")
        if (params.length >= 1) {
          for (i <- 1 to params(0).toInt) {
            println(s"\tFind of ${i}")
            c.find(i)
          }
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      println("Please provide approach {n, t, p} and the number of disjoint sets as such: <approach #n>")
      val line = StdIn.readLine().split(" ")
      if (line.size >= 2)
        startDisjointSetShell(line)
    }
  }
}