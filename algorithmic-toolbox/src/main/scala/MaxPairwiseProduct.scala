import scala.io.StdIn

object MaxPairwiseProduct {
  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt
    val nums = StdIn.readLine().split(' ').map(BigInt(_))
    val numSet = nums.toSet
    val numList = nums.toList.sorted
    println(numList.takeRight(2).product)
  }
}
