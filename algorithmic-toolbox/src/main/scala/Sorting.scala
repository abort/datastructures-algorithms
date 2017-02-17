import scala.io.StdIn
import scala.util.Random

object Sorting {
  def main(args: Array[String]): Unit = {
    // ignore line 1
    StdIn.readLine
    val elements = StdIn.readLine.split(' ').map(_.toInt)

    quicksort(elements)
    println(elements.mkString(" "))
  }

  def quicksort(elements : Array[Int]) : Unit = {
    def quicksort(elements : Array[Int], lo : Int, hi : Int) : Unit = {
      if (lo >= hi) return

      assignPivot(elements, lo, hi)

      val (lt, gt) = partition(elements, lo, hi)
      quicksort(elements, lo, lt - 1)
      quicksort(elements, gt + 1, hi)
    }

    def assignPivot(elements : Array[Int], lo : Int, hi : Int) : Unit = {
      // Nevermind seeding for this toy example
      val pivotIndex = Random.nextInt(hi - lo + 1) + lo
      exchange(elements, lo, pivotIndex)
    }

    def exchange(elements : Array[Int], x : Int, y : Int) : Unit = {
      val temp = elements(x)
      elements(x) = elements(y)
      elements(y) = temp
    }

    def partition(elements : Array[Int], lo : Int, hi : Int) : (Int, Int) = {
      val pivot = elements(lo)
      var lessThan = lo + 1
      var greaterThan = hi
      var i = lo + 1
      var j = hi
      while (i <= greaterThan) {
        val value = elements(i)

        if (value < pivot) {
          exchange(elements, i, lessThan)
          i += 1
          lessThan += 1
        }
        else if (value > pivot) {
          exchange(elements, i, greaterThan)
          greaterThan -= 1
        }
        else {
          i += 1
        }
      }

      (lessThan, greaterThan)
    }

    quicksort(elements, 0, elements.length - 1)
  }
}
