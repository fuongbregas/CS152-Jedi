package expressions

import value._
import ui._

case class FunCall(operator: Expression, operands: List[Expression]) extends Expression {

  def execute(env: Environment): Value = {
    val useStatic: Boolean = false

    var env2 = env;
    if (!useStatic) 
      env2 = null
      
    var temp = operands.map(_.execute(env))
    
    try {
      if (operator.execute(env).isInstanceOf[Closure]) operator.execute(env).asInstanceOf[Closure].apply(temp, env2)
      else throw new UndefinedException
    } catch {
      case e: UndefinedException => system.execute(operator.asInstanceOf[Identifier], temp)
    }
  }
}

