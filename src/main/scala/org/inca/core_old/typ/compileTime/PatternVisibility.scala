package org.inca.core_old.typ.compileTime

import org.inca.core_old.content.IPatternVisibility

// todo can't just use traits as 'object' - should this be done this way?
case class PatternVisibility(visible: Boolean) extends IPatternVisibility
