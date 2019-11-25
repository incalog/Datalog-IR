package org.inca.core.`type`.compileTime

import org.inca.core.content.IPatternVisibility

// todo can't just use traits as 'object' - should this be done this way?
case class PatternVisibility(visible: Boolean) extends IPatternVisibility
