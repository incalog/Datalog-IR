package org.inca.generators.gp

import org.inca.lang.core.Constraints.IPathElement
import org.inca.lang.core.Content.{IPatternBody, IPatternBodyContent, TemporaryVariable}
import org.inca.lang.core.Reference.VariableReference
import org.inca.lang.core.Typp.Typ
import org.inca.lang.core.Values.{IValue, IVariableValue}
import org.inca.lang.gp.Constraints.PathExpressionConstraint
import org.inca.lang.gp.Content.{GraphPattern, GraphPatternBody, IGraphPatternBodyContent}
import org.inca.lang.gp.Virtual.ParentPathElement

object Transformers {
  def transformPattern(pattern: GraphPattern): GraphPattern = {
    val transformedBodies: Seq[IPatternBody] = for(body <- pattern.bodies) yield {
      transformBodies(body)
    }
    pattern.copy(bodies = transformedBodies)
  }

  private def transformBodies(body: IPatternBody): IPatternBody = {
      GraphPatternBody(body.contents.flatMap(content => transformContent(content)))
  }

  // one line of one body
  private def transformContent(content: IPatternBodyContent): Seq[IPatternBodyContent] = {
    content match {
      case p: PathExpressionConstraint =>
        if (p.element.next.isDefined) {
          recursiveStuff(p.element, p.src, p.trg, p.typ, 0)
        }else {
          Seq(content)
        }
    }


  }

  // add PathExpressionConstraint p from previous method to this
  private def recursiveStuff(elem: IPathElement,
                             src: IVariableValue,
                             trg: IValue,
                             typ: Typ,
                             depth: Int): Seq[IGraphPatternBodyContent] = {
    if (elem.next.isEmpty) {
      val pec = elem match {
        case pp: ParentPathElement =>
          src match {
            case vr: VariableReference =>
              PathExpressionConstraint (vr, trg, pp.copy (next = None), typ)
            case tv: TemporaryVariable =>
              PathExpressionConstraint (VariableReference(tv), trg, pp.copy (next = None), typ)
          }
      }
      Seq(pec)
    } else {
      // create PathExpression Constraint
      // create temp vars and carry them to the next call
      val temp = TemporaryVariable(s"${elem.link.toString}_$depth", Some(typ))

      val pec = elem match {
        case pp: ParentPathElement =>
        src match {
          case vr: VariableReference =>
            PathExpressionConstraint (vr, temp, pp.copy (next = None), typ)
          case tv: TemporaryVariable =>
            PathExpressionConstraint (VariableReference(tv), temp, pp.copy (next = None), typ)
        }
      }
      Seq(pec) ++ recursiveStuff(elem.next.get, temp, trg, typ, depth + 1)
    }
  }
}
