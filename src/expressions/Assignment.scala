package expressions
import value._
import ui._

case class Assignment(id: Identifier, vals: Expression) extends SpecialForm {
  def execute(env: Environment): Value = {

    var f = id.execute(env)
    if (f.isInstanceOf[Variable]) {

      f.asInstanceOf[Variable].content = vals.execute(env)

      Notification.done

    } else
      throw new TypeException("Must be Variable")
  }
}