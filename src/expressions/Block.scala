package expressions
import value._

case class Block (locals: List[Expression]) extends SpecialForm
{
  // The execute method creates a temporary environment that extends env. 
  // Each expression in locals is executed relative to this environment.
  // Add a new block parser to the parser and test your implementation.
  
  
    def execute(env: Environment) = {
      var tempEnvironment = new Environment(env)
      for(i <- 0 until locals.length - 1){
        locals(i).execute(tempEnvironment)
      }
      locals(locals.length - 1).execute(tempEnvironment)
    }
}