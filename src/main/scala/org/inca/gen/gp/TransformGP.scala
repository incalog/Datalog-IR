package org.inca.gen.gp

import org.inca.lang.Core._
import org.inca.lang.Gp._
import org.inca.meta.MetaElements.NodeType

object TransformGP {
  def transformPattern: GraphPattern => GraphPattern = (pattern: GraphPattern) =>
    pattern.copy(bodies =  pattern.bodies map { transformBodies })

  private def transformBodies(body: PatternBody): PatternBody =
    GraphPatternBody(body.contents.zipWithIndex flatMap(
        content => transformContent(content._1, content._2)))

  private def transformContent(content: PatternBodyContent, line: Int): Seq[PatternBodyContent] =
    content match {
      case PathExpressionConstraint(src, trg, elem, typ) =>
        if (elem.next.isDefined) splitExp(elem, src, trg, typ, 0, line)
        else Seq(content)
      case _ => Seq(content)
    }

  private def splitExp(elem: PathElement,
                               src: VariableValue,
                               trg: Value,
                               typ: NodeType,
                               depth: Int,
                               line: Int): Seq[GraphPatternBodyContent] =
    if (elem.next.isEmpty) Seq(matcher(elem, trg, typ, src))
    else {
      val temp = TemporaryVariable(s"${elem.link.fld.toString.substring(
        elem.link.fld.toString.lastIndexOf('.') + 1)}_${line}_$depth", Some(typ))
      Seq(matcher(elem, temp, typ, src)) ++ splitExp(elem.next.get, temp, trg, typ, depth + 1, line)
    }

  private def matcher(elem: PathElement,
                      value: Value,
                      typ: NodeType,
                      src: VariableValue): PathExpressionConstraint = elem match {
      case pp: PathElementImpl => src match {
        case vr: VariableReference =>
          PathExpressionConstraint(vr, value, pp.copy(next = None), elem.link.nodeType)
        case tv: TemporaryVariable =>
          PathExpressionConstraint(VariableReference(tv), value, pp.copy(next = None), elem.link.nodeType)
      }
    }
}
