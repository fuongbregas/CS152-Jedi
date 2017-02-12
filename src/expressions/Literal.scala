package expressions
import value._

trait Literal extends Expression with Value with Serializable{
  def execute( env : Environment):  Value = 
    this
}
