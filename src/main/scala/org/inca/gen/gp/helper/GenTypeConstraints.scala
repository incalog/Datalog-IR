package org.inca.gen.gp.helper

import org.inca.lang.core.Constraints.{EqualityCompareFeature, InequalityCompareFeature}
import org.inca.lang.core.Content.{CoreTemporaryVariable, IParameter, IPatternBodyContent}
import org.inca.lang.core.Reference.CoreVariableReference
import org.inca.lang.core.Values.{IValue, IVariableValue}
import org.inca.lang.gp.Constraints._
import org.inca.lang.gp.Content.GraphPatternParameter
import org.inca.gen.Gensym._
import org.inca.gen.gp.model.Prefix._

import scala.meta._
import Util._
import org.inca.gen.gp.model.{IntegerConstant, LongConstant, Primitive}
import org.inca.meta.MetaElements.MetaElement


object GenTypeConstraints {

  def typeConstraintsParameters(graphParameters: Seq[IParameter]): List[Stat] =
    graphParameters.toList map { param =>
      q"""new TypeConstraint(body,
        Tuples.flatTupleOf(${Term.Name(s"var_${param.name}")}),
         new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[${asTypeSelect(param.typ.get.toString)}]))
       )"""
    }

  def contextPointers(names: List[String]): List[Stat] =
    names map { name =>
      q"""new TypeConstraint(
         body,
         Tuples.flatTupleOf(${Term.Name(s"var__$name")}),
         new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[org.inca.lang.core.Constraints.ContextPointer]))
       )"""
    }

  def typeConstraints(bodyContent: Seq[IPatternBodyContent]): List[Stat] =
  bodyContent.toList collect {
      case pxc: PathExpressionConstraint      => pathExpressionConstraint(pxc)
      case pcc: PatternCompositionConstraint  => patternCompositionConstraint(pcc)
      case gcc: GraphPatternCompareConstraint => graphPatternCompareConstraint(gcc)
      case ccc: GraphPatternConceptConstraint => patternConceptConstraint(ccc)
      // todo check constraint
    }

  private def pathExpressionConstraint(pxc: PathExpressionConstraint): Stat = {
    val src = pxc.src.variable match {
      case CoreTemporaryVariable(name, _) => Term.Name(s"var__$name")
      case GraphPatternParameter(name, _) => Term.Name(s"var_$name")
    }
    val trg = pxc.trg match {
      case CoreVariableReference(v) => Term.Name(s"var_${v.name}")
      case CoreTemporaryVariable(name, _) => Term.Name(s"var__$name")
    }
    q"""new TypeConstraint(body,
        Tuples.staticArityFlatTupleOf($src, $trg),
        new TFInputKey.NodeLinkKey(MetaElements.NodeType(
           classOf[${asTypeSelect(pxc.typ.toString)}])
             (${Lit.String(pxc.element.link.fld.getName)}))
      )"""
  }

  private def patternCompositionConstraint(pcc: PatternCompositionConstraint): Stat =
    q"""new PositivePatternCall(body,
          Tuples.flatTupleOf(..${getVariableReference(pcc.call.arguments)}),
          ${Term.Name(s"${pcc.call.pattern.name}")}.instance().getInternalQueryRepresentation
       )
     """

  private def patternConceptConstraint(ccc: GraphPatternConceptConstraint): Stat =
    q"""new TypeConstraint(body,
          Tuples.flatTupleOf(..${getVariableReference(Seq(ccc.vari))}),
          new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[${asTypeSelect(ccc.typ.toString)}]))
       )
     """


  private def getVariableReference(args: Seq[IValue]): List[Term] =
    args.toList map {
      case CoreVariableReference(v) => v match {
        case GraphPatternParameter(name, _) => Term.Name(s"var_$name")
        case CoreTemporaryVariable(name, _) => Term.Name(s"var__$name")
      }
      case CoreTemporaryVariable(name, _) => Term.Name(s"var_$name")
    }

  private def graphPatternCompareConstraint(cc: GraphPatternCompareConstraint): Stat =
    matchCompareConstraint(cc, termNameLabel(cc.left), termNameLabel(cc.right))

  private def matchCompareConstraint(compare: GraphPatternCompareConstraint,
                                     left: Term.Name, right: Term.Name): Stat =
  compare.feature match {
    case _: EqualityCompareFeature => q"new Equality(body, $left, $right)"
    case _: InequalityCompareFeature => q"new Inequality(body, $left, $right)"
  }

  private def termNameLabel(value: Any): Term.Name =
  value match {
    case CoreVariableReference(v) => v match {
      case GraphPatternParameter(_, _) => Term.Name(s"var_${v.name}")
      case CoreTemporaryVariable(_, _) => Term.Name(s"var__${v.name}")
    }
    case _ => Term.Name(generateLabel(var__, value))
  }
}