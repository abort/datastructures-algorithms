import scala.io.StdIn

object KnuthMorrisPrat {
  def main(args: Array[String]): Unit = {
    val pattern = StdIn.readLine
    val genome = StdIn.readLine

    println(computeOccurrences(pattern, genome).mkString(" "))
  }

  private def computeOccurrences(p : String, text : String) : Iterable[Int] = {
    val input = {
      val s = new StringBuilder(p)
      s.append('$')
      s.append(text)
      s.toArray
    }
    val patternToSeparatorLength = p.length
    val doublePatternLength = 2 * p.length
    val s = prefix(input)
    s.drop(patternToSeparatorLength).collect { case (value, index) if value == patternToSeparatorLength => index - doublePatternLength }
  }

  @inline private def prefix(s : Array[Char]) : Array[(Int, Int)] = {
    val len = s.length
    val borders = Array.ofDim[(Int, Int)](s.length)
    borders(0) = (0, 0)
    var k = 0
    for (i <- 1 until len) {
      while (k > 0 && s(k) != s(i)) {
        k = borders(k - 1)._1
      }
      if (s(k) == s(i)) {
        k = k + 1
      }
      else {
        k = 0
      }

      borders(i) = (k, i)
    }
    borders
  }
}
