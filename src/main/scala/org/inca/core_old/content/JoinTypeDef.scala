package org.inca.core_old.content
import org.inca.core_old.typ.ICompileTimeIncAType

case class JoinTypeDef(name: String, types: List[ICompileTimeIncAType]) extends IJoinTypeDef
