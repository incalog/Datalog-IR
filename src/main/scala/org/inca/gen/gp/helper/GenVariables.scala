package org.inca.gen.gp.helper

import org.inca.gen.Gensym
import org.inca.gen.gp.helper.Util._
import org.inca.lang.Core._
import org.inca.lang.Gp._
import org.inca.lang.Values._

import scala.meta._

object GenVariables {

  def createTemporaryVariables(names: List[String]): List[Stat] =
    names.map { name =>
      q"val ${Pat.Var(Term.Name(s"var__$name"))}: PVariable = body.getOrCreateVariableByName(${Lit.String(name)})"
    }

  def localGlobalVariables(graphParameters: Seq[Parameter]): List[Stat] =
    graphParameters.map { gp =>
      q"val ${Pat.Var(Term.Name(s"var_${gp.name}"))}: PVariable = body.getOrCreateVariableByName(${Lit.String(gp.name)})"
    }.toList

  def temporaryVariables(body: Seq[PatternBodyContent]): List[String] =
    body.collect {
      case PathExpressionConstraint(src, trg, _, _) =>
        List[String](
          trg match {
            case TemporaryVariable(name, _) => name
            case _ => ""
          },
          hasRefVar(src)
        )
      case CompareConstraint(_, left, right) =>
        List[String](
          hasRefVar(left),
          hasRefVar(right))
    }.toList.flatten.distinct.filterNot(x => x.isEmpty)

  def generatePrimitives(): List[Stat] = Gensym.variables.map { value =>
    val variable = Pat.Var(Term.Name("var__" + value._2))
    //    val reference = Term.Apply(asTermSelect(value._1.getClass.toString.substring(1).split(".").toList), List(Term.Name(value._1.toString)))
    val reference = value._1 match {
      case v: BooleanLiteral => Lit.Boolean(v.value)
      case v: IntegerLiteral => Lit.Int(v.value)
      case v: LongLiteral => Lit.Long(v.value)
      case v: StringLiteral => Lit.String(v.value)
    }
      Term.Apply(asTermSelect2(value._1), List(Term.Name(value._1.toString)))
    q"val $variable = body.newConstantVariable($reference)"
  }.toList

  private def asTermSelect2(value: Any): Term.Select = {
    asTermSelect(value.getClass.toString.substring(6).split('.').toList)
    //    pathList
    //      .drop(2)
    //      .foldLeft(Term.Select(Term.Name(pathList.head), Term.Name(pathList.tail.head)))
    //      { (inner, outer) => Term.Select(inner, Term.Name(outer)) }
    //    Term.Select(Term.Name("A"), Term.Name("B"))
  }

  def registerValues(bodies: Seq[PatternBody]): Unit = bodies.foreach { body =>
      body.contents.foreach {
        case CompareConstraint(_, left, right) =>
          left match {
            case v: LiteralValue => Gensym.register(v)
            case _ => ()
          }
          right match {
            case v: LiteralValue => Gensym.register(v)
            case _ => ()
          }
        case _ => ()
      }
    }

  def pparams(graphParameters: Seq[Parameter]): List[Stat] =
    graphParameters.toList map { gp =>
      val name = s"p_${gp.name}"
      val primitiveTypeName = asTypeSelect(gp.typ.get.toString)
      val pConceptKey = q"new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[$primitiveTypeName]))"

      q"""private val ${Pat.Var(Term.Name(name))}: PParameter =
            new PParameter(${Lit.String(gp.name)},
            MetaElements.NodeType(classOf[$primitiveTypeName]).toString,
            $pConceptKey)"""
    }

  private def hasRefVar(v: Any): String = v match {
    case VariableReference(v) => v match {
      case TemporaryVariable(name, _) => name
      case _ => ""
    }
    case _ => ""
  }
}
