import scala.io.StdIn

object PhoneBook {
  def main(args: Array[String]): Unit = {
    val a = 34
    val b = 2
    val p = 10000019

    val n = StdIn.readInt
    val table = Array.fill[Option[String]](p)(None)

    def hash(number : Int) : Int = (a * number + b) % p
    def processCommand(command: Command): Unit = {
      command match {
        case Add(num, name) => table(hash(num)) = Some(name)
        case Del(num) => table(hash(num)) = None
        case Find(num) => println(table(hash(num)).getOrElse("not found"))
      }
    }

    for (i <- 1 to n) {
      processCommand(readCommand)
    }
  }

  def readCommand() : Command = {
    val line = StdIn.readLine.split(' ')
    // we assume correct input
    line(0) match {
      case "add" => Add(line(1).toInt, line(2))
      case "del" => Del(line(1).toInt)
      case "find" => Find(line(1).toInt)
      case _  => readCommand
    }
  }

  trait Command
  case class Add(number : Int, name : String) extends Command
  case class Del(number : Int) extends Command
  case class Find(number : Int) extends Command
}
