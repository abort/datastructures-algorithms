import scala.io.StdIn

object Inversions {
  def main(args: Array[String]): Unit = {
    // ignore line 1
    StdIn.readLine
    val elements = StdIn.readLine.split(' ').map(_.toInt)

    println(countInversions(elements))
  }

  case class Element(value : Int, inversions : Int) extends Ordered[Element] {
    override def compare(that: Element) = value.compare(that.value)

    override def toString: String = s"<$value, $inversions>"
  }

  def countInversions(initialElements : Array[Int]) : Int = {
    def countInversions(elements : Array[Element]) : Array[Element] = {
      if (elements.length <= 1) return elements

      val toTake = elements.length / 2
      val left = countInversions(elements.take(toTake))
      val right = countInversions(elements.takeRight(elements.length - toTake))
      merge(left, right)
    }

    def merge(left : Array[Element], right : Array[Element]): Array[Element] = {
      // println("We merge!")

      val total = left.length + right.length
      val result = Array.ofDim[Element](total)
      var r = 0
      var i = 0
      var j = 0
      while (r < total) { // i < left.length && j < right.length
        lazy val leftElement = left(i)
        lazy val rightElement = right(j)

        if (i == left.length) {
          result(r) = rightElement
          j += 1
        }
        else if (j == right.length) {
          result(r) = leftElement
          i += 1
        }
        else if (leftElement > rightElement) {
          result(r) = Element(rightElement.value, rightElement.inversions + 1)
          j += 1
        }
        else if (leftElement <= rightElement) {
          result(r) = leftElement
          i += 1
        }

        r += 1
      }

      // val rx = result.mkString("[", " ", "]")
      // println(s"Result: $rx")

      result
    }

    val invs = countInversions(initialElements.map(Element(_, 0)))
    println(s"$invs")
    invs.map(_.inversions).sum
  }
}
