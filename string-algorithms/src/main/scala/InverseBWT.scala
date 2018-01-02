import scala.io.StdIn

object InverseBWT {
  val delimiter = '$'
  def main(args: Array[String]): Unit = println(computeInverseBurrowsWheelerTransform(StdIn.readLine))

  def computeInverseBurrowsWheelerTransform(input : String) : String = {
    val firstColumnsZipped = input.zipWithIndex
    val first = firstColumnsZipped.sortBy(_._1)
    val firstToLast = first.zip(firstColumnsZipped).toMap

    var pointer = first.head
    val result = Array.ofDim[Char](input.length)
    for (i <- 0 until input.length) {
      result(input.length - i - 1) = pointer._1
      pointer = firstToLast(pointer)
    }

    String.valueOf(result)
  }

  private def sort(a: Array[Char], min: Char, max: Char): Array[Char] = {
    def key(value: Char): Int = value - min

    val result: Array[Char] = new Array[Char](a.length)

    // Count how many of each key we have
    val count: Array[Int] = new Array[Int](max - min + 1)
    a.foreach( (e: Char) => count(key(e)) += 1)

    // Add preceding counts to compute offset for each key
    for (i <- 1 to (max - min)) {
      count(i) += count(i - 1)
    }

    // Assemble results using offset and sorted keys
    for (e <- a.reverseIterator) {
      count(key(e)) -= 1
      result(count(key(e))) = e
    }
    result
  }
}
