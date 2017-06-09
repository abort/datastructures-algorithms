import java.io.{BufferedReader, InputStreamReader, OutputStreamWriter, PrintWriter}
import java.util.StringTokenizer

import scala.collection.mutable.ArrayBuffer

object MergingTables {
  object InputReader {
    val reader : BufferedReader = new BufferedReader(new InputStreamReader(System.in))
    var tokenizer : StringTokenizer = null

    private def next() : String = {
      while (tokenizer == null || !tokenizer.hasMoreTokens) {
        tokenizer = new StringTokenizer(reader.readLine())
      }
      tokenizer.nextToken()
    }

    def nextInt() : Int = next().toInt
  }

  def main(args: Array[String]): Unit = {
    val out = new PrintWriter(new OutputStreamWriter(System.out))

    val n = InputReader.nextInt
    val mergeOperations = InputReader.nextInt
    val tables = Array.ofDim[Table](n + 1)
    tables(0) = Table(0, 0)
    var initialMax = Int.MinValue
    for (i <- 1 to n) {
      tables(i) = Table(i, InputReader.nextInt())
      initialMax = Math.max(initialMax, tables(i).rows)
    }
    val merges = Array.ofDim[(Int, Int)](mergeOperations)
    for (i <- merges.indices) {
      merges(i) = (InputReader.nextInt(), InputReader.nextInt())
    }

    val maxTableSizes = mergeTables(tables, initialMax, merges)
    maxTableSizes.foreach(out.println)
    out.close()

  }

  case class Table(id : Int, var rows : Int) extends Ordered[Table] {
    var symlink : Option[Table] = None
    var rank : Int = 0

    override def compare(that: Table) = rows - that.rows
  }

  def mergeTables(tables : Array[Table], initialMax : Int, merges : Array[(Int, Int)]) : Seq[Int] = {
    def find(x : Table): Table = {
      if (x.symlink.isDefined) {
        x.symlink = Some(find(x.symlink.get)) // path compression
        find(x.symlink.get)
      }
      else x
    }

    def union(x : Table, y : Table) : Unit = {
      val xRoot = find(x)
      val yRoot = find(y)

      if (xRoot == yRoot) return

      if (xRoot.rank < yRoot.rank) {
        xRoot.symlink = Some(yRoot)
        yRoot.rows += xRoot.rows
        xRoot.rows = 0
      }
      else if (xRoot.rank >= yRoot.rank) {
        yRoot.symlink = Some(xRoot)
        xRoot.rows += yRoot.rows
        yRoot.rows = 0
        if (xRoot.rank == yRoot.rank)
          xRoot.rank += 1
      }
    }

    val maxTableSizes = ArrayBuffer.empty[Int]
    var maxSize = initialMax

    for (op <- merges) {
      val dest = find(tables(op._1))
      val src = find(tables(op._2))

      union(dest, src)
      maxSize = Math.max(Math.max(src.rows, dest.rows), maxSize)

      maxTableSizes += maxSize
    }

    maxTableSizes
  }
}