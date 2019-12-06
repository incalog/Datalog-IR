package org.inca.gp

import org.inca.core_old.typ.ICompileTimeIncAType
import org.inca.core_old.content._

object Content {
  trait IGraphPatternBodyContent extends IPatternBodyContent
  trait IGraphPatternModuleContent extends IPatternModuleContent

  case class EmptyGraphPatternContent() extends EmptyContent with IGraphPatternBodyContent with IGraphPatternModuleContent
  case class GraphPattern(name: String,
                          parameters: Seq[IParameter],
                          bodies: Seq[IPatternBody],
                          visibility: Option[IPatternVisibility]) extends IPattern with IGraphPatternModuleContent
  case class GraphPatternBody(contents: Seq[IPatternBodyContent]) extends IPatternBody
  case class GraphPatternParameter(name: String, typ: Option[ICompileTimeIncAType]) extends IParameter
}
