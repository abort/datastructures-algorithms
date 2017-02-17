import scala.io.StdIn

object MajorityElement {
  def main(args: Array[String]): Unit = {
    // ignore line 1
    StdIn.readLine
    val votes = StdIn.readLine.split(' ').map(_.toInt).toVector

    println(computeMajorityVote(votes))
  }

  def computeMajorityVote(votes: Vector[Int]) : Int = {
    def computeMajorityVote(votes: Vector[Int], lo : Int, hi : Int) : Int = {
      def countVotes(vote : Int, lo : Int, hi : Int, minimum : Int) : Int = {
        val counts = votes.slice(lo, hi).foldLeft(0) { (sum, value) => if (value == vote) sum + 1 else sum }
        if (counts >= minimum) vote else -1
      }

      if (lo == hi) -1
      else if (lo + 1 == hi) votes(lo)
      else {
        val mid = lo + (hi - lo) / 2
        val leftMajority = computeMajorityVote(votes, lo, mid)
        val rightMajority = computeMajorityVote(votes, mid, hi)
        lazy val minimum = (hi - lo) / 2 + 1
        // println(s"Left majority $leftMajority, right majority $rightMajority")
        // Count votes if we have a majority
        if (leftMajority != -1) {
          if (countVotes(leftMajority, lo, hi, minimum) > -1) return leftMajority
        }

        if (rightMajority != -1) {
          if (countVotes(rightMajority, lo, hi, minimum) > -1) return rightMajority
        }
        -1
      }
    }

    if (computeMajorityVote(votes, 0, votes.length) == -1) 0 else 1
  }
}
