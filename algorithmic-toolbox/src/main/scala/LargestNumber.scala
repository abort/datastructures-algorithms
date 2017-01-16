import scala.annotation.tailrec
import scala.io.StdIn

object LargestNumber {
  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt

    val numbers = StdIn.readLine().split(' ').map(_.toInt)
    println(computeLargestNumber(n, numbers).mkString)
  }

  def computeLargestNumber(n : Int, numbers : Array[Int]) : Array[Int] = {
    def toArray(i : Int) : Array[Int] = {

      @tailrec
      def computeDigits(value : Int, digits : Int) : Int = {
        if (value > 0) computeDigits(value / 10, digits + 1)
        else digits
      }

      val digits = computeDigits(i, 0)
      val output = Array.ofDim[Int](digits)
      var remainder = i
      for (d <- (0 until digits).reverse) {
        val divisor = math.pow(10.0, d).toInt
        val index = digits - d - 1
        // println(s"$remainder divided by $divisor at $index")
        output(index) = remainder / divisor
        remainder = remainder % divisor
      }

      output
    }

    val decomposedNumbers = numbers.map(toArray(_))
    decomposedNumbers.sortWith { case (a : Array[Int], b : Array[Int]) => {
      val firstCompound = a ++ b
      val secondCompound = b ++ a
      var first = 0
      var second = 0
      for (d <- (0 until firstCompound.length).reverse) {
        val index = firstCompound.length - d - 1
        first += firstCompound(index) * math.pow(10, d).toInt
        second += secondCompound(index) * math.pow(10, d).toInt
      }
      // println(s"First: $first second: $second")
      first > second
    }}.flatten
  }
}
