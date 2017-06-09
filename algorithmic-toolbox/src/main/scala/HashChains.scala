import scala.io.StdIn

object HashChains {
  def main(args: Array[String]): Unit = {
    val x = 263
    val p = 1000000007


    val m = StdIn.readInt
    val n = StdIn.readInt
    val table = Array.fill[Seq[String]](m)(Seq.empty)

    def hash(s : String) : Int = {
      val code = s.zipWithIndex.foldRight(0.toLong) { case ((c, i), acc) => (acc * x + c.toInt) % p } % m
      code.toInt
    }
    def handleCommand() : Unit = {
      val line = StdIn.readLine.split(' ')
      // we assume correct input
      line(0) match {
        case "add" => {
          val name = line(1)
          val hashcode = hash(name)
          if (!contains(name)) table(hashcode) = name +: table(hashcode)
        }
        case "del" => {
          val name = line(1)
          val hashcode = hash(name)
          if (contains(name)) table(hashcode) = table(hashcode).filter(!_.equals(name))
        }
        case "find" if contains(line(1)) => println("yes")
        case "find" => println("no")
        case "check" => println(table(line(1).toInt).mkString(" "))
        case _ => handleCommand()
      }
    }
    def contains(name : String) : Boolean = table(hash(name)).contains(name)

    for (i <- 1 to n) {
      handleCommand
    }
  }
}
