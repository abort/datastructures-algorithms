import scala.io.StdIn

object APlusB {
  def main(args: Array[String]): Unit = {
    val line = StdIn.readLine()
    val nums = line.split(' ').map(_.toInt)
    val sum = nums.sum
    println(sum)
  }
}