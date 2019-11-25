package org.inca.gp.content

import org.inca.core.content.Comment

case class GraphPatternComment(override var text: String)
  extends Comment(text)
    with IGraphPatternBodyContent
    with IGraphPatternModuleContent
