package expressions
import value._
trait Expression {
  def execute(env: Environment): Value
}
