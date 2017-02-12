package value
import expressions._
import scala.collection.mutable.HashMap
import value._
import ui._


class Environment(var nextEnv: Environment = null) extends HashMap[Identifier, Value] with Value {
  def find(id: Identifier): Value = {
    if (!this.contains(id) && nextEnv != null) {
      nextEnv.find(id)
    }
    else if (!this.contains(id) && nextEnv == null) {
      Notification.undefined
    }
    else {
      this.get(id).get
    }
  }
  
  def put(ids: List[Identifier], values: List[Value]) {

    for (i <- 0 until Math.min(ids.length, values.length)) {
      put(ids(i), values(i))
    }

  }
  
}
