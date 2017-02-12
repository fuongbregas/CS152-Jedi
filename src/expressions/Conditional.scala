package expressions
import value._
case class Conditional(val condition: Expression, val consequence: Expression, val alternative: Expression = null) extends SpecialForm {
  def execute(env: Environment): Value = {
    if (condition.execute(env).toString() == "true") 
      consequence.execute(env)
    else if (condition.execute(env).toString() == "false") 
      alternative.execute(env)
    else 
      Notification.undefined
  }
}