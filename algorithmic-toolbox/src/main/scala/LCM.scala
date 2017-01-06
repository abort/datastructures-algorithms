import scala.io.StdIn

object LCM {
  def main(args: Array[String]): Unit = {
    val line = StdIn.readLine().split(' ').map(BigInt(_))
    println(lcm(line(0), line(1)))
  }

  def lcm(a : BigInt, b : BigInt) : BigInt = {
    (a * b) / gcd(a, b)
  }
  def gcd(a : BigInt, b : BigInt) : BigInt = {
    // Matches are slower
    if (b == 0) a
    else gcd(b, a % b)
  }
}
