package org.inca.core.content

import org.inca.AST

trait IPatternBody extends AST {
  var contents: List[IPatternBodyContent]
}
