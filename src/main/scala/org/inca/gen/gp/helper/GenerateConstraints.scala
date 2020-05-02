package org.inca.gen.gp.helper

import org.inca.gen.Gensym
import org.inca.gen.gp.helper.Util._
import org.inca.lang.Core._
import org.inca.lang.Gp._

import scala.meta._


object GenerateConstraints {

  def typeConstraints(bodyContent: Seq[PatternBodyContent]): List[Stat] =
    bodyContent.toList collect {
      case pxc: PathExpressionConstraint => pathExpressionConstraint(pxc)
      case pcc: CompositionConstraint => patternCompositionConstraint(pcc)
      case gcc: CompareConstraint => graphPatternCompareConstraint(gcc)
      case ccc: ConceptConstraint => patternConceptConstraint(ccc)
    }

  def generateParameters(graphParameters: Seq[Parameter]): List[Stat] =
    graphParameters.toList map { param =>
      q"""new TypeConstraint(body,
        Tuples.flatTupleOf(${Term.Name(s"var_${param.name}")}),
         new TFInputKey.NodeTypeKey(MetaElements.NodeType(
         classOf[${toImportStatement(param.typ.get.toString)}])))"""
    }

  private def pathExpressionConstraint(pxc: PathExpressionConstraint): Stat = {
    val src = pxc.src.variable match {
      case TemporaryVariable(name, _) => Term.Name(s"var__$name")
      case GraphPatternParameter(name, _) => Term.Name(s"var_$name")
    }
    val trg = pxc.trg match {
      case VariableReference(v) => Term.Name(s"var_${v.name}")
      case TemporaryVariable(name, _) => Term.Name(s"var__$name")
    }
    q"""new TypeConstraint(body,
        Tuples.staticArityFlatTupleOf($src, $trg),
        new TFInputKey.NodeLinkKey(MetaElements.NodeType(
           classOf[${toImportStatement(pxc.typ.toString)}])
             (${Lit.String(pxc.element.link.fld.getName)})))"""
  }

  private def patternCompositionConstraint(pcc: CompositionConstraint): Stat =
    q"""new PositivePatternCall(body,
          Tuples.flatTupleOf(..${getVariableReference(pcc.call.arguments)}),
          ${Term.Name(s"${pcc.call.pattern.name}")}.instance().getInternalQueryRepresentation)"""

  private def patternConceptConstraint(ccc: ConceptConstraint): Stat =
    q"""new TypeConstraint(body,
          Tuples.flatTupleOf(..${getVariableReference(Seq(ccc.vari))}),
          new TFInputKey.NodeTypeKey(MetaElements.NodeType(
            classOf[${toImportStatement(ccc.typ.toString)}])))"""

  private def graphPatternCompareConstraint(cc: CompareConstraint): Stat =
    matchCompareConstraint(cc, termNameLabel(cc.left), termNameLabel(cc.right))

  private def getVariableReference(args: Seq[Value]): List[Term] = args.toList map {
      case VariableReference(v) => v match {
        case GraphPatternParameter(name, _) => Term.Name(s"var_$name")
        case TemporaryVariable(name, _) => Term.Name(s"var__$name")
      }
      case TemporaryVariable(name, _) => Term.Name(s"var__$name")
      case lit: LiteralValue => Term.Name(s"var__${Gensym.variables(lit)}")
    }

  private def matchCompareConstraint(compare: CompareConstraint,
                                     left: Term.Name,
                                     right: Term.Name): Stat = compare.feature match {
      case _: EqualityCompareFeature => q"new Equality(body, $left, $right)"
      case _: InequalityCompareFeature => q"new Inequality(body, $left, $right)"
    }

  private def termNameLabel(value: Value): Term.Name = value match {
      case VariableReference(v) => v match {
        case GraphPatternParameter(_, _) => Term.Name(s"var_${v.name}")
        case TemporaryVariable(_, _) => Term.Name(s"var__${v.name}")
      }
      case TemporaryVariable(name, _) => Term.Name(s"var__$name")
      case v: LiteralValue => Term.Name("var__" + Gensym.variables(v))
    }
}