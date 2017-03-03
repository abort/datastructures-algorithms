import scala.io.StdIn

object PlacingParentheses {
  def main(args: Array[String]): Unit = {
    println(computeMaximumValue(StdIn.readLine))
  }

  def computeMaximumValue(formula : String) : Long = {
    var operators = IndexedSeq[Char]()
    var digits = IndexedSeq[Int]()

    // We created our data structures
    formula.zipWithIndex.foreach {
      case (c, i) => {
        if (i % 2 == 0) digits = digits :+ c.asDigit
        else operators = operators :+ c
      }
    }

    println(s"operators: ${operators}")
    println(s"digits: ${digits}")
    val minima = Array.ofDim[Long](digits.length, digits.length)
    val maxima = Array.ofDim[Long](digits.length, digits.length)
    for (i <- 0 until digits.length) {
      minima(i)(i) = digits(i)
      maxima(i)(i) = digits(i)
    }

    def minAndMax(i : Int, j : Int) : (Long, Long) = {
      var min = Long.MaxValue
      var max = Long.MinValue

      for (k <- i to j - 1) {
        println(s"formula for ${digits(i)} and ${digits(j)} with op ${formula(k*2+1)}")
        val a = evaluateFormula(maxima(i)(k), formula(k * 2 + 1), maxima(k + 1)(j))
        val b = evaluateFormula(maxima(i)(k), formula(k * 2 + 1), minima(k + 1)(j))
        val c = evaluateFormula(minima(i)(k), formula(k * 2 + 1), minima(k + 1)(j))
        val d = evaluateFormula(minima(i)(k), formula(k * 2 + 1), maxima(k + 1)(j))
        val seq = Seq(a, b, c, d)
        min = Math.min(seq.min, min)
        max = Math.min(seq.max, max)
      }

      println(s"min: ${min} max: ${max}")
      (min, max)
    }

    for (s <- 0 until digits.length) {
      for (i <- 0 until digits.length - s) {
        val j = i + s
        val result = minAndMax(i, j)
        minima(i)(j) = result._1
        maxima(i)(j) = result._2
      }
    }

    maxima(0)(digits.length - 1)
  }

  def evaluateFormula(a : Long, op : Char, b : Long) : Long = op match {
    case '*' => a * b
    case '/' => a / b
    case '-' => a - b
    case '+' => a + b
    case _ => ???
  }
}
