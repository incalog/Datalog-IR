package inca

import inca.backend.ir.Datalog.Pattern

package object debugger {
  // type Value = Any
  type Tuple = Seq[Value]
  type PartialTuple = Map[String, Value]
  type Relation = Map[Pattern, Tuple]
}
