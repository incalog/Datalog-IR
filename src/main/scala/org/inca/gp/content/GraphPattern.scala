package org.inca.gp.content

import org.inca.core.content.{IParameter, IPattern, IPatternBody, IPatternVisibility}

case class GraphPattern(var name: String,
                        var parameters: List[IParameter],
                        var bodies: List[IPatternBody],
                        var visibility: Option[IPatternVisibility])
  extends IPattern
  with IGraphPatternModuleContent
