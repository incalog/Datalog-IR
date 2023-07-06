package inca.frontend.objectoriented.interpreter

import inca.frontend.objectoriented.interpreter.Value.NULL

sealed trait Value {
  def isUnit: Boolean = false
  def isNull: Boolean = this match {
    case ObjectValue(NULL.`oid`) => true
    case _ => false
  }

  def asBoolean: Boolean = throw new IllegalStateException(s"Expected boolean but got $this")
  def asScala: Any = this
  def asTuple: Seq[Value] = throw new IllegalStateException(s"Expected tuple but got $this")
  def asObject(heap: RuntimeHeap): Object = throw new IllegalStateException(s"Expected object but got $this")
  def asSet: Set[Value] = throw new IllegalStateException(s"Expected set but got $this")
}

final case class ScalaValue(v: Any) extends Value {
  override def asBoolean: Boolean = v match {
    case b: Boolean => b
    case _ => super.asBoolean
  }

  override def asScala: Any = v
}

final case class TupleValue(values: Seq[Value]) extends Value {
  override def isUnit: Boolean = values.isEmpty

  private def fullFlatten(s: Seq[Any]): Seq[Any] = s.flatten {
    case s: Seq[_] => fullFlatten(s)
    case v => Seq(v)
  }

  //override def asScala: Any = fullFlatten(values.map(_.asScala))
  override def asScala: Product = {
    val clazz = Class.forName("scala.Tuple" + values.size)
    clazz.getConstructors.apply(0).newInstance(values: _*).asInstanceOf[Product]
  }
  override def asTuple: Seq[Value] = values
}

final case class SetValue(values: Set[Value]) extends Value {
  override def asScala: Any = values.map(_.asScala).toSet
  override def asSet: Set[Value] = values
  def size: Int = values.size
}

final case class ObjectValue(id: Int) extends Value {
  override def asScala: Any = this
  override def asObject(heap: RuntimeHeap): Object = heap(id)
}
final case class StructuralObjectValue(o: Object) extends Value {
  override def asScala: Any = this
  override def asObject(heap: RuntimeHeap): Object = o
}

final class Object(val cls: String, val oid: Int, val fvals: Map[String, Value], val isStructural: Boolean, val isMono: Boolean) {
  def isNull: Boolean = oid == 0

  def asValue: Value =
    if (isStructural)
      StructuralObjectValue(this)
    else
      ObjectValue(oid)

  def updated(fname: String, v: Value) =
    new Object(cls, oid, fvals + (fname -> v), isStructural, isMono)

  override def equals(obj: Any): Boolean = obj match {
    case obj : Object if obj.isNull => this.isNull
    case obj : Object if obj.isStructural => cls == obj.cls && fvals == obj.fvals
    // Mono types must be stable. It would be enough to just check the result field
    case obj : Object => cls == obj.cls && oid == obj.oid && fvals == obj.fvals
    // Checking fvals guarantees that we do not terminate if we mutate inside a fixpoint
    case _ => false
  }

  override def hashCode(): Int =
    if (this.isStructural)
      fvals.hashCode()
    else
      oid

  override def toString: String = s"$cls($oid, $isStructural, $isMono, $fvals)"
}

object Object {
  def unapply(obj: Object): Option[(String, Int, Map[String, Value])] = Some((obj.cls, obj.oid, obj.fvals))
  def apply(name: String, id: Int, fvals: Map[String, Value], isStructural: Boolean, isMono: Boolean): Object = new Object(name, id, fvals, isStructural, isMono)
}

object SetValue {
  def apply(): SetValue = new SetValue(Set())
}

object Value {
  val UNIT: TupleValue = TupleValue(Seq())
  val NULL: Object = Object("Null", 0, Map(), false, false)
  val TRUE: ScalaValue = ScalaValue(true)
  val FALSE: ScalaValue = ScalaValue(false)
}
