package org.inca.lang

import org.inca.lang.Core.Value

object Values {

  trait LiteralValue extends Value {
    val value: Any
  }

  trait BooleanLiteral extends LiteralValue {
    val value: Boolean
  }

  trait IntegerLiteral extends LiteralValue {
    val value: Int
  }

  trait LongLiteral extends LiteralValue {
    val value: Long
  }

  trait StringLiteral extends LiteralValue {
    val value: String
  }

  //  case class EnumLit(value: Enumeration) extends LiteralValue
  //  case class EnumMemberLit(value: LiteralValue) extends LiteralValue
}
