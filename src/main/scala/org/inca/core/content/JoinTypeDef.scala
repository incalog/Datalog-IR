package org.inca.core.content
import org.inca.core.`type`.ICompileTimeIncAType

case class JoinTypeDef(name: String, types: List[ICompileTimeIncAType]) extends IJoinTypeDef
