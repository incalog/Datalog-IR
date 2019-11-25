package org.inca.core.content

trait IPattern {
  var parameters: List[IParameter]
  var bodies: List[IPatternBody]
  var visibility: Option[IPatternVisibility]
}
