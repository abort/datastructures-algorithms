import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object FibonacciLastDigit {
  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt()
    println(fibonacci(n))
  }

  def fibonacci(n: Int): Int = {
    if (n == 0) return 0
    val fibos = Array(0, 1)
    for (i <- fibos.length to n) {
      val oldLastFibo = fibos(1)
      fibos(1) = (fibos(1) + fibos(0)) % 10
      fibos(0) = oldLastFibo
    }
    fibos(1)
  }
}
