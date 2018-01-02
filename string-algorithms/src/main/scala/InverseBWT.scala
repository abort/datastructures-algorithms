import scala.io.StdIn

object InverseBWT {
  val delimiter = '$'
  def main(args: Array[String]): Unit = println(computeInverseBurrowsWheelerTransform(StdIn.readLine))

  def computeInverseBurrowsWheelerTransform(input : String) : String = {
    val firstColumns = sort(input.toCharArray, input.min, input.max)
    val firstColumnsZipped = firstColumns.zipWithIndex
    val lastColumns = input.toIndexedSeq

    val indexedCharToChar = collection.mutable.Map[(Char, Int), (Char, Int)](firstColumnsZipped.map(_ -> null) : _*)
    val elements = firstColumns.map((_, 0))
    val indexedFirstChars = collection.mutable.Map(elements : _*)
    val indexedLastChars = collection.mutable.Map(elements : _*)

    firstColumnsZipped.foreach { case (c, i) =>
      val occurrence = indexedFirstChars(c)
      val subject = lastColumns(i)
      var subjectOccurrence = indexedLastChars(subject)
      indexedCharToChar((c, occurrence)) = (subject, subjectOccurrence)
      indexedFirstChars(c) = occurrence + 1
      indexedLastChars(subject) = subjectOccurrence + 1
    }

    val delimitingKey = (delimiter, 0)
    var key = delimitingKey
    val result = new StringBuilder(input.length)
    do {
      val entry = indexedCharToChar(key)
      val next = entry

      result.insert(0, key._1)
      key = next
    } while (key != delimitingKey)

    result.toString
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
