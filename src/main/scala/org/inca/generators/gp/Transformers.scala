package org.inca.generators.gp

import org.inca.lang.core.Constraints.IPathElement
import org.inca.lang.core.Content.{IPatternBody, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.core.Typp.Typ
import org.inca.lang.core.Values.{IValue, IVariableValue}
import org.inca.lang.gp.Constraints.PathExpressionConstraint
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, IGraphPatternBodyContent}
import org.inca.lang.gp.Element.GeneratedParameter
import org.inca.lang.gp.Virtual.ParentPathElement

object Transformers {
  def transformPattern(pattern: GraphPattern): GraphPattern = {
    val transformedBodies: Seq[IPatternBody] = for (body <- pattern.bodies) yield {
      transformBodies(body)
    }
    pattern.copy(bodies = transformedBodies)
  }

  private def transformBodies(body: IPatternBody): IPatternBody = {
    GraphPatternBody(
      body.contents.zipWithIndex.flatMap(
        content => transformContent(content._1, content._2)
      ))
  }

  private def transformContent(content: IPatternBodyContent, line: Int): Seq[IPatternBodyContent] = {
    content match {
      case p: PathExpressionConstraint =>
        if (p.element.next.isDefined) {
          splitNestedPathExpressions(p.element, p.src, p.trg, p.typ, 0, line)
        } else {
          Seq(content)
        }
    }
  }

  private def splitNestedPathExpressions(elem: IPathElement,
                             src: IVariableValue,
                             trg: IValue,
                             typ: Typ,
                             depth: Int,
                             line: Int): Seq[IGraphPatternBodyContent] = {
    if (elem.next.isEmpty) {
      Seq(matcher(elem, trg, typ, src))
    } else {
      val temp = new TemporaryVariable(s"${elem.link.toString}_${line}_$depth", Some(typ)) with GeneratedParameter
      Seq(matcher(elem, temp, typ, src)) ++ splitNestedPathExpressions(elem.next.get, temp, trg, typ, depth + 1, line)
    }
  }

  private def matcher(elem: IPathElement, value: IValue, typ: Typ, src: IVariableValue): PathExpressionConstraint = {
    elem match {
      case pp: ParentPathElement =>
        src match {
          case vr: VariableReference =>
            PathExpressionConstraint(vr, value, pp.copy(next = None), typ)
          case tv: TemporaryVariable =>
            PathExpressionConstraint(VariableReference(tv), value, pp.copy(next = None), typ)
        }
    }
  }
}
