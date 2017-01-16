import scala.io.StdIn

object Change {
  def main(args: Array[String]): Unit = {
    while (true) {
      val n = StdIn.readInt()
      println(change(n))
    }
  }

  def change(n : Int) : Int = {
    val denominate = (amount : Int, coins : Int, denominator : Int) => (amount % denominator, amount / denominator + coins)
    var result = denominate(n, 0, 10)
    result = denominate(result._1, result._2, 5)
    result = denominate(result._1, result._2, 1)
    result._2
  }
}
