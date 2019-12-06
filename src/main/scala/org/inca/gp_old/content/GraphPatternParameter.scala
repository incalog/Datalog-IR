package org.inca.gp_old.content

import org.inca.core.typ.ICompileTimeIncAType
import org.inca.core.content.IParameter

case class GraphPatternParameter(name: String, typ: ICompileTimeIncAType) extends IParameter
