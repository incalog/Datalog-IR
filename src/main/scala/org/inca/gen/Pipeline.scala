package org.inca.gen

import org.inca.gen.gp.TransformGP.transformPattern
import org.inca.gen.gp.GeneratorGP.querySpecification
import org.inca.lang.gp.Content.GraphPattern

import scala.meta.Source

object Pipeline {

  def generateGraphPattern(pattern: GraphPattern): Source =
    (transformPattern _ andThen
      querySpecification)(pattern)

}
