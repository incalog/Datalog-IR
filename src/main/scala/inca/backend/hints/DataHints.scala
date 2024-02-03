package inca.backend.hints
import inca.backend.hints.Hint.Key

object DataHints {

  val ConstructorKey = "DATA_CONSTRUCTOR"
  val SelectorKey = "DATA_SELECTOR"
  val DataTypeKey = "DATA_TYPE"
  val IDBConstructorKey = "IDB_DATA_CONSTRUCTOR"
  val DataTypeNameKey = "Data_Type_Name"

  /**
   * The annotated pattern represents a constructor.
   */
  object Constructor extends Hint {
    override def key: Key = ConstructorKey
  }

  /**
   * The annotated pattern represents a selector.
   */
  object Selector extends Hint {
    override def key: Key = SelectorKey
  }

  /**
   * The annotated pattern represents a data type.
   */
  object DataType extends Hint {
    override def key: Key = DataTypeKey
  }

  /**
   * The annotated body represents a rule to construct a value during run time
   */
  object IDBConstructor extends Hint {
    override def key: Key = IDBConstructorKey
  }

  case class DataTypeName(name: String) extends Hint {
    override def key: Key = DataTypeNameKey
  }
}
