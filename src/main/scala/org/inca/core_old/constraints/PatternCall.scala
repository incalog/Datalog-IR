package org.inca.core_old.constraints
import org.inca.core_old.content.IPattern
import org.inca.core_old.values.IValue

case class PatternCall(transitive: Boolean, arguments: Seq[IValue], pattern: IPattern) extends IPatternCall