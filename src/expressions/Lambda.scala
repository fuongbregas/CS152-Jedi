package expressions

import value._


// As with all expression subclasses, Lambda should be a case class with all fields declared as constructor parameters. 
// The execute method simply creates and returns a closure.
case class Lambda(id: List[Identifier], exp: Expression) extends SpecialForm{
  def execute(env: Environment): Value = {
    new Closure(id, exp, env)    
    
  }
}