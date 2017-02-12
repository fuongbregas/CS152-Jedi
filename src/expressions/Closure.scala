package expressions
import expressions._
import value._
import ui._

// This method creates a temporary environment extending the defining environment and containing 
// the bindings of parameters to arguments. (Throw a type exception if these two lists don't have the same length.) 
// The body is executed relative to this environment.

class Closure(params: List[Identifier], body: Expression, defEnv: Environment) extends Value {
  def apply(args: List[Value], lambdaEnv: Environment = null): Value = {
    
    var tempEnv = new Environment(defEnv)
    
    if(lambdaEnv == null) 
      tempEnv = new Environment(defEnv)
    else
      tempEnv = lambdaEnv

    // Exe env
    if (args.length == params.length) {
      tempEnv.put(params, args)
      body.execute(tempEnv)
    } else
      throw new JediException("Jedi Error")
  }
}