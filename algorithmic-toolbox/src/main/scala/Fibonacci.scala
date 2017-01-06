import scala.collection.mutable.ArrayBuffer
import scala.io.StdIn

object Fibonacci {
  def main(args: Array[String]): Unit = {
    val n = StdIn.readInt()
    println(fibonacci(n))
  }

  def fibonacci(n : Int) : Int = {
    val fibbos = ArrayBuffer(0, 1)
    for (i <- fibbos.length to n) {
      fibbos += fibbos(i - 1) + fibbos(i - 2)
    }
    fibbos(n)
  }
}
