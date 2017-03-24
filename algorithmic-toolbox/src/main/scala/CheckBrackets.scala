import scala.collection.mutable.Stack
import scala.io.StdIn

object CheckBrackets {
  def main(args: Array[String]): Unit = {
    val line = StdIn.readLine
    val stack = Stack[Bracket]()
    for ((char, index) <- line.zipWithIndex) {
      val position = index + 1
      if (Set('(', '{', '[') contains char) {
        stack.push(Bracket(char, position)) // 1 based index
      }

      else if (Set(')', '}', ']') contains char) {
        if (stack.isEmpty || !stack.pop.doesMatch(char)) {
          println(position)
          return
        }
      }
    }
    if (stack.isEmpty) {
      println("Success")
    }
    else {
      println(stack.pop.position)
    }
  }

  case class Bracket(char : Char, position : Integer) {
    def doesMatch(otherChar : Char) : Boolean = {
      if (char == '[' && otherChar == ']') return true
      else if (char == '(' && otherChar == ')') return true
      else if (char == '{' && otherChar == '}') return true
      false
    }
  }
}
