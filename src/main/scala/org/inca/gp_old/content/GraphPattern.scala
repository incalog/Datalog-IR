package org.inca.gp_old.content

import org.inca.core_old.content.{IParameter, IPattern, IPatternBody, IPatternVisibility}

case class GraphPattern(var name: String,
                        var parameters: List[IParameter],
                        var bodies: List[IPatternBody],
                        var visibility: Option[IPatternVisibility])
  extends IPattern
  with IGraphPatternModuleContent
