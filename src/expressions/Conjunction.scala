package expressions
import value._
import ui._

case class Conjunction(var operands: List[Expression]) extends SpecialForm {
  def execute(env: Environment): Value = {
    var result = true    
    for (i <- 0 until operands.length - 1) {
      var check = operands(i).execute(env)
      if (check.isInstanceOf[Boole]){
        result = check.asInstanceOf[Boole].value        
      }
      else{
        throw new TypeException("Not Boole")        
      }
    }    
    new Boole(result)
  }
}