package value
import expressions._
case class Boole(val value: Boolean)extends Literal with Value {
  def ||(other : Boole):Boole =  {
    if (this.value || other.value) 
      new Boole(true) 
    else 
      new Boole(false)
  }
  def &&(other : Boole):Boole= {
    if (this.value && other.value) 
      new Boole(true) 
    else 
      new Boole(false)
  }
  def !():Boole = {
    if (this.value) 
      new Boole(false) 
    else 
      new Boole(true)
  }
  override def toString() = {
    value.toString()
  }
}
