package ui
import expressions._
import value._
object console {
  val parsers = new SithParsers // for now
  val globalEnv = new Environment()

  def execute(cmmd: String): String = {
    val tree = parsers.parseAll(parsers.expression, cmmd)

    tree match {
      case t: parsers.Failure => throw new SyntaxException(t)
      case _                  => "" + tree.get.execute(globalEnv)
    }
  }

  def repl {
    // declare locals
    var more = true
    while (more) {
      try {

        print("-> ")
        var cmd = readLine
        if (cmd == "quit") {
          println("Bye");
          more = false
        } else {
          println(execute(cmd).toString())
        }

      } catch {
        case e: SyntaxException => {
          println(e.msg)
          println(e.result.msg)
          println("line # = " + e.result.next.pos.line)
          println("column # = " + e.result.next.pos.column)
          println("token = " + e.result.next.first)
        }
        // handle other types of exceptions
        case e: UndefinedException => {
          println(e.msg)
        }
        
        case e: TypeException=> {
          println(e.msg())
        }

      } finally {
        Console.flush
      }
    }
  }

  def main(args: Array[String]): Unit = { repl }
}

