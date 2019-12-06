package org.inca.gp_old.content

import org.inca.core.content.EmptyContent

case class EmptyGraphPatternContent()
  extends EmptyContent()
  with IGraphPatternBodyContent
  with IGraphPatternModuleContent
