package org.inca.lang.gp

import org.inca.lang.core.Content._
import org.inca.meta.MetaElements.MetaElement


object Content {
  trait IGraphPatternBodyContent extends IPatternBodyContent
  trait IGraphPatternModuleContent extends IPatternModuleContent

  abstract class PatternParameter(name: String, typ: Option[MetaElement])

  case class EmptyGraphPatternContent() extends EmptyContent with IGraphPatternBodyContent with IGraphPatternModuleContent
  case class GraphPattern(name: String,
                          parameters: Seq[IParameter],
                          bodies: Seq[IPatternBody],
                          visibility: Option[IPatternVisibility]) extends IPattern with IGraphPatternModuleContent
  case class GraphPatternBody(contents: Seq[IPatternBodyContent]) extends IPatternBody
  case class GraphPatternParameter(name: String, typ: Option[MetaElement])
    extends PatternParameter(name, typ) with IParameter

  case class GraphPatternComment(text: String) extends Comment(text) with IGraphPatternBodyContent with IGraphPatternModuleContent

  case class VirtualGraphPattern(name: String) extends IPattern {
    override val parameters: Seq[IParameter] = Seq()
    override val bodies: Seq[IPatternBody] = Seq()
    override val visibility: Option[IPatternVisibility] = None
  }
}
