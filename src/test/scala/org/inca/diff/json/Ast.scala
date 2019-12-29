package org.inca.diff.json

import org.inca.diff.macros.{diffableType,diffableConstr}

// adapted from https://github.com/lihaoyi/fastparse/blob/master/fastparse/test/src/fastparse/JsonTests.scala

@diffableType trait Js {
  def value: Any
}
object Js {
  case class Str(value: java.lang.String) extends Js
  case class Obj(value: Seq[JSField]) extends Js
  case class Arr(value: Seq[Js]) extends Js
  case class Num(value: Double) extends Js
  case object False extends Js {
    def value = false
  }
  case object True extends Js {
    def value = true
  }
  case object Null extends Js {
    def value = null
  }
}

@diffableType trait JSField
object JSField {
  case class Field(name: java.lang.String, value: Js) extends JSField
}

