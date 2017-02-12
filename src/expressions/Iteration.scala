package expressions
import value._
import ui._

case class Iteration(condition: Expression, exp: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {
    var result: Value = Notification.undefined
    var f = condition.execute(env)
    if (f.isInstanceOf[Boole]) {

      while (condition.execute(env).toString() == "true") {
        result = exp.execute(env)
      }
      result
    } else
      throw new TypeException("Type Exception error")

  }

}