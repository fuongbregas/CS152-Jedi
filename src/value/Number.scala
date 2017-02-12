package value
import expressions._

case class Number(val value: Double) extends Literal with Value {
   
  def +(other: Number):Number = Number(this.value + other.value)  
  def -(other: Number):Number = Number(this.value - other.value)
  def *(other: Number):Number = Number(this.value * other.value)
  def /(other: Number):Number = Number(this.value / other.value)
  
  def <(other: Number):Boole = {
    if(this.value < other.value)
      new Boole(true)
    new Boole(false)
  }
  def ==(other: Number):Boole = {
    if(this.value == other.value)
      new Boole(true)
    new Boole(false)
  }
  
  def >(other: Number):Boole = {
    if(this.value < other.value)
      new Boole(false)
    new Boole(true)
  }
  
  override def toString() = value.toString()
}
