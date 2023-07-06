package inca.frontend.objectoriented.interpreter

sealed trait Value {
  def isUnit: Boolean = false
  def asBoolean: Boolean = throw new IllegalStateException(s"Expected boolean but got $this")
  def asScala: Any = this
  def asTuple: Seq[Value] = throw new IllegalStateException(s"Expected tuple but got $this")
  def asObject: (String, Int, Map[String, Value]) = throw new IllegalStateException(s"Expected object but got $this")
  def asSet: Set[Value] = throw new IllegalStateException(s"Expected set but got $this")
  def updateObject(fname: String, fval: Value): Unit = {}
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

final class ObjectValue(val cls: String, val id: Int, var fvals: Map[String, Value]) extends Value {
  override def asObject: (String, Int, Map[String, Value]) = (cls, id, fvals)
  override def updateObject(fname: String, fval: Value): Unit = fvals += fname -> fval

  override def asScala: Any = this
  def isStructural: Boolean = id == 0
  def isNull: Boolean = id == -1
  var isMono: Boolean = false

  override def equals(obj: Any): Boolean = obj match {
    case obj : ObjectValue if obj.isNull => this.isNull
    case obj : ObjectValue if obj.isStructural => cls == obj.cls && fvals == obj.fvals
    // Mono types must be stable. It would be enough to just check the result field
    case obj : ObjectValue if obj.isMono => cls == obj.cls && id == obj.id && fvals == obj.fvals
    // Checking fvals guarantees that we do not terminate to early if we mutate inside a fixpoint using monotones
    // If we do not check the fields we can use normal mutation in fixpoints
    case ObjectValue(cls, id, fvals) =>
      // Monotones must be stable
      def getMonoFields(fields: Map[String, Value]): Map[String, Value] = fields.filter {
        case (_, obj: ObjectValue) => obj.isMono
        case _ => false
      }
      this.cls == cls && this.id == id && getMonoFields(this.fvals) == getMonoFields(fvals)
    case _ => false
  }

  override def hashCode(): Int =
    if (this.isStructural)
      fvals.hashCode()
    else
      id
}

object ObjectValue {
  def unapply(obj: ObjectValue): Option[(String, Int, Map[String, Value])] = Some((obj.cls, obj.id, obj.fvals))
  def apply(name: String, id: Int, fvals: Map[String, Value]): ObjectValue = new ObjectValue(name, id, fvals)
}

object SetValue {
  def apply(): SetValue = new SetValue(Set())
}

object Value {
  val UNIT: TupleValue = TupleValue(Seq())
  val NULL: ObjectValue = ObjectValue("Null", -1, Map())
  val TRUE: ScalaValue = ScalaValue(true)
  val FALSE: ScalaValue = ScalaValue(false)
}
