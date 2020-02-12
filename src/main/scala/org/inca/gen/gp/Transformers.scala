package org.inca.gen.gp

import org.inca.lang.core.Constraints.IPathElement
import org.inca.lang.core.Content.{IPatternBody, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.core.Values.{IValue, IVariableValue}
import org.inca.lang.gp.Constraints.PathExpressionConstraint
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, IGraphPatternBodyContent}
import org.inca.lang.gp.Element.GeneratedParameter
import org.inca.lang.gp.Virtual.ParentPathElement
import org.inca.meta.MetaElements.NodeType

object Transformers {
  def transformPattern(pattern: GraphPattern): GraphPattern = {
    val transformedBodies: Seq[IPatternBody] = pattern.bodies.map { body => transformBodies(body) }
    pattern.copy(bodies = transformedBodies)
  }

  private def transformBodies(body: IPatternBody): IPatternBody =
    GraphPatternBody(body.contents.zipWithIndex.flatMap(
        content => transformContent(content._1, content._2)))

  private def transformContent(content: IPatternBodyContent, line: Int): Seq[IPatternBodyContent] =
    content match {
      case PathExpressionConstraint(src, trg, elem, typ) =>
        if (elem.next.isDefined) {
          splitNestedPathExpressions(elem, src, trg, typ, 0, line)
        } else {
          Seq(content)
        }
      case _ => Seq(content)
    }

  private def splitNestedPathExpressions(elem: IPathElement,
                                         src: IVariableValue,
                                         trg: IValue,
                                         typ: NodeType,
                                         depth: Int,
                                         line: Int): Seq[IGraphPatternBodyContent] =
    if (elem.next.isEmpty) {
      Seq(matcher(elem, trg, typ, src))
    } else {
      val temp = new TemporaryVariable(s"${elem.link.toString}_${line}_$depth", Some(typ))
        with GeneratedParameter
      Seq(matcher(elem, temp, typ, src)) ++
        splitNestedPathExpressions(elem.next.get, temp, trg, typ, depth + 1, line)
    }

  private def matcher(elem: IPathElement,
                      value: IValue,
                      typ: NodeType,
                      src: IVariableValue): PathExpressionConstraint =
    elem match {
      case pp: ParentPathElement => src match {
        case vr: VariableReference =>
          PathExpressionConstraint(vr, value, pp.copy(next = None), typ)
        case tv: TemporaryVariable =>
          PathExpressionConstraint(VariableReference(tv), value,
            pp.copy(next = None), typ)
      }
    }
}
