package org.inca.gp_old.content

import org.inca.core_old.content.Comment

case class GraphPatternComment(override var text: String)
  extends Comment(text)
    with IGraphPatternBodyContent
    with IGraphPatternModuleContent
