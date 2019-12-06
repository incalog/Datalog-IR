package org.inca.core.content
import org.inca.core.typ.ICompileTimeIncAType

case class JoinTypeDef(name: String, types: List[ICompileTimeIncAType]) extends IJoinTypeDef
