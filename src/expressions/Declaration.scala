package expressions

import ui._
import value._
case class Declaration(var id: Identifier, var exp: Expression) extends SpecialForm {
  def execute( env : Environment): Value = { 
    env.put(id, exp.execute(env)); 
    Notification.ok 
  }
}