package ui
import scala.util.parsing.combinator._

class TypeException (opcode: String = "Type Error") extends JediException(opcode) {
  def msg(): String = "Type Exception"
}