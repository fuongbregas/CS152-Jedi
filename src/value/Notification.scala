package value


class Notification(val message:String) extends Value{
  override def toString() = message

}
object Notification{
  def apply(msg:String) = new Notification(msg)
  val updated = new Notification("variable updated")
  val binding = new Notification("Binding Created")
  val done = Notification("DONE")
  val ok = Notification("OK")
  val undefined = Notification("UNDEFINED")
}
