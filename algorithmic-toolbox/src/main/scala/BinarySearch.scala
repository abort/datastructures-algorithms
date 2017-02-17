import scala.io.StdIn

object BinarySearch {
  def main(args: Array[String]): Unit = {
    val readLine = () => StdIn.readLine().split(' ').map(_.toInt)

    // line 1
    val haystackLine = readLine()
    val haystack = haystackLine.tail

    // line 2
    val needlesLine = readLine()
    val needles = needlesLine.tail

    println(needles.map(binarySearch(haystack, _)).mkString(" "))
  }

  def binarySearch(haystack : Array[Int], needle : Int): Int = {
    def binarySearch(haystack : Array[Int], needle : Int, lo : Int, hi : Int): Int = {
      lazy val mid = lo + (hi - lo) / 2
      lazy val pick = haystack(mid)

      if (lo > hi) -1
      else if (pick == needle) mid
      else if (needle < pick) binarySearch(haystack, needle, lo, mid - 1)
      else binarySearch(haystack, needle, mid + 1, hi)
    }

    binarySearch(haystack, needle, 0, haystack.size - 1)
  }
}
