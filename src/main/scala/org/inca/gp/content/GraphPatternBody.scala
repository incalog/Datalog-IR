package org.inca.gp.content

import org.inca.core.content.{IPatternBody, IPatternBodyContent}

case class GraphPatternBody(contents: List[IPatternBodyContent]) extends IPatternBody
