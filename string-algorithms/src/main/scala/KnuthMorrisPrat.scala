import scala.io.StdIn

object KnuthMorrisPrat {
  def main(args: Array[String]): Unit = {
    val pattern = StdIn.readLine
    val genome = StdIn.readLine

    println(computeOccurences(pattern, genome).mkString(" "))
  }

  private def computeOccurences(p : String, text : String) : Iterable[Int] = {
    val input : String = {
      val s = new StringBuilder(p)
      s.append('$')
      s.append(text)
      s.toString
    }
    val s = prefix(input)
    val patternToSeparatorLength = p.length
    val doublePatternLength = 2 * p.length
    s.zipWithIndex.drop(patternToSeparatorLength).filter { case (value, _) => value == p.length }.map(_._2 - doublePatternLength)
  }

  private def prefix(p : String) : IndexedSeq[Int] = {
    val s = p.toCharArray
    val len = s.length
    var borders = Vector.fill(len)(0)
    var k = 0
    for (i <- 1 until len) {
      while (k > 0 && s(k) != s(i)) {
        k = borders(k - 1)
      }
      if (s(k) == s(i)) {
        k = k + 1
      }
      else {
        k = 0
      }

      borders = borders.updated(i, k)
    }
    borders
  }
}
