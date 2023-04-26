package inca.runtime.data.objectoriented

case class StructuralID(override val typ: String, fields: Map[String, Any]) extends Identity(typ) {
  override def readField[T](name: String): T = {
    this.fields(name).asInstanceOf[T]
  }
}

object StructuralID {
  def apply(typ: String, fields: (String, Any)*): StructuralID = {
    // In case of flattened tuple values we might get duplicated keys
    new StructuralID(typ, fields.toMap)
  }
}