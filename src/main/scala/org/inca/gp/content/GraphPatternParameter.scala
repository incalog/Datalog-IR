package org.inca.gp.content

import org.inca.core.`type`.ICompileTimeIncAType
import org.inca.core.content.IParameter

case class GraphPatternParameter(name: String, `type`: ICompileTimeIncAType) extends IParameter
