import scala.io.StdIn

object BurrowsWheelerTransform {
  def main(args: Array[String]): Unit = println(computeBurrowsWheelerTransform(StdIn.readLine))

  def computeBurrowsWheelerTransform(s : String) : String = {
    val cycles = computeCycles(s)
    cycles.sorted.foldLeft("")(_ + _.last)
  }

  private def computeCycles(s : String) : IndexedSeq[String] = s.indices.map(i => s.substring(i) + s.substring(0, i))
}
