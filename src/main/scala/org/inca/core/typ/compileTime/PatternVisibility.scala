package org.inca.core.typ.compileTime

import org.inca.core.content.IPatternVisibility

// todo can't just use traits as 'object' - should this be done this way?
case class PatternVisibility(visible: Boolean) extends IPatternVisibility
