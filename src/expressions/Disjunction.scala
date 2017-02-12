package expressions
import value._
import ui.JediException



case class Disjunction(var operands: List[Expression]) extends SpecialForm{
  def execute(env: Environment): Value = {
    var result = false
    for (i <- 0 until operands.length - 1){
      var check = operands(i).execute(env) 
      if(check.isInstanceOf[Boole])
        result = true
      else
        throw new JediException("Not Boole")
    }    
    new Boole(result)
  }  
}