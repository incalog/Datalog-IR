package org.inca.gp.content

import org.inca.core.content.EmptyContent

case class EmptyGraphPatternContent()
  extends EmptyContent()
  with IGraphPatternBodyContent
  with IGraphPatternModuleContent
