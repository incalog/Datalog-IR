package org.inca.core.constraints
import org.inca.core.content.IPattern
import org.inca.core.values.IValue

case class PatternCall(transitive: Boolean, arguments: Seq[IValue], pattern: IPattern) extends IPatternCall