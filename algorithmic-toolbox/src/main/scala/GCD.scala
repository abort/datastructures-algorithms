import scala.io.StdIn

object GCD {
  def main(args: Array[String]): Unit = {
    val line = StdIn.readLine().split(' ').map(_.toInt)
    println(gcd(line(0), line(1)))
  }

  def gcd(a : Int, b : Int) : Int = {
    // Matches are slower
    if (b == 0) a
    else gcd(b, a % b)
  }
}
