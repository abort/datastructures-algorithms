import scala.io.StdIn

object HashSubString {
  def main(args: Array[String]): Unit = {
    val x = 263
    val p = 1000000007
    val m = 20

    def hash(s : String) : Int = {
      val code = s.zipWithIndex.foldRight(0.toLong) { case ((c, i), acc) => (acc * x + c.toInt) % p } % m
      println(s"Hashing $s = $code")
      code.toInt
    }

    val pattern = StdIn.readLine
    val text = StdIn.readLine

    val patternHash = hash(pattern)
    val initialHash = pattern.foldLeft(1.toLong) { case (acc, _) => (acc * x) % p }

    val textDelta = text.length - pattern.length
    val precomputedHashes = Array.ofDim[Int](textDelta + 1)
    val lastSubString = text.substring(textDelta) // - 1?
    precomputedHashes(textDelta) = hash(lastSubString)

    for (i <- (textDelta - 1) to 0 by -1) {
      precomputedHashes(i) = ((x * precomputedHashes(i + 1) + text.charAt(i).toInt - initialHash * text.charAt(i + pattern.length).toInt) % p).toInt
    }

    println(s"Precomputed hashes: ${precomputedHashes.mkString(" ")}")

    var results : Seq[Int] = Seq.empty
    for (i <- 0 to textDelta) {
      if (patternHash == precomputedHashes(i) && pattern.equals(text.take(pattern.length))) {
        results = results :+ i
      }
    }

    println(results.mkString(" "))
  }
}
