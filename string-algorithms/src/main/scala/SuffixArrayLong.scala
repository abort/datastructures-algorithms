import scala.io.StdIn

object SuffixArrayLong {
  private val AlphabetSize = 5

  def main(args: Array[String]): Unit = {
    println(computeSuffixArray(StdIn.readLine.toUpperCase).mkString(" "))
  }

  private def computeSuffixArray(s: String) : Iterable[Int] = {
    var order = sortCharacters(s)
    var classes = computeCharClasses(s, order)
    var l = 1
    while (l < s.length) {
      order = sortDoubled(s, l, order, classes)
      classes = updateClasses(order, classes, l)
      l = l * 2
    }

    order
  }

  private def charToIndex(c : Char) : Int = c match {
    case '$' => 0
    case 'A' => 1
    case 'C' => 2
    case 'G' => 3
    case 'T' => 4
    case _ => ???
  }

  private def sortCharacters(s: String) : IndexedSeq[Int] = {
    val order = Array.ofDim[Int](s.length)
    val count = Array.fill[Int](AlphabetSize)(0)
    s.foreach { char => count(charToIndex(char)) += 1 }
    count.indices.tail.foreach { j =>
      count(j) += count(j - 1)
    }
    s.indices.reverse.foreach { i =>
      val char = s(i)
      val index = charToIndex(char)
      count(index) -= 1
      order(count(index)) = i
    }
    order
  }

  private def computeCharClasses(s : String, order : IndexedSeq[Int]) = {
    val classes = Array.ofDim[Int](s.length)
    classes(order.head) = 0
    s.indices.tail.foreach { i =>
      if (s(order(i)) != s(order(i - 1))) classes(order(i)) = classes(order(i - 1)) + 1
      else classes(order(i)) = classes(order(i - 1))
    }
    classes
  }

  private def sortDoubled(s : String, l : Int, order : IndexedSeq[Int], classes : IndexedSeq[Int]) = {
    val count = Array.fill[Int](s.length)(0)
    val newOrder = Array.ofDim[Int](s.length)
    s.indices.foreach { i =>
      count(classes(i)) += 1
    }

    s.indices.tail.foreach { i =>
      count(i) = count(i) + count(i - 1)
    }

    s.indices.reverse.foreach { i =>
      val start = (order(i) - l + s.length) % s.length
      val cls = classes(start)
      count(cls) -= 1
      newOrder(count(cls)) = start
    }
    newOrder
  }

  private def updateClasses(newOrder : IndexedSeq[Int], classes : IndexedSeq[Int], l : Int) = {
    val n = newOrder.length
    val newClasses = Array.ofDim[Int](n)
    newClasses(newOrder.head) = 0

    for (i <- 1 until n) {
      val current = newOrder(i)
      val prev = newOrder(i - 1)
      val mid = (current + l) % n
      val midPrev = (prev + l) % n
      if (classes(current) != classes(prev) || classes(mid) != classes(midPrev)) {
        newClasses(current) = newClasses(prev) + 1
      }
      else {
        newClasses(current) = newClasses(prev)
      }
    }
    newClasses
  }
}

