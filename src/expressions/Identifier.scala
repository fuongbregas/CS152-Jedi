package expressions

import value._
import ui._
import javax.management.Notification
case class Identifier(val name: String) extends Expression with Serializable 
{
  def execute(env: Environment): Value = {
    if (env.find(this) == value.Notification.undefined) 
      throw new UndefinedException("Undefined Identifier: " + name)
    else env.find(this)
  }
}
