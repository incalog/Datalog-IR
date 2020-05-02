package org.inca.gen.gp.helper

import org.inca.gen.Gensym
import org.inca.gen.gp.helper.Util._
import org.inca.lang.Core._
import org.inca.lang.Gp._

import scala.meta._

object GenerateVariables {

  def createTemporaryVariables(names: List[String]): List[Stat] = names.map { n =>
    q"val ${varName("var__", n)}: PVariable = body.getOrCreateVariableByName(${Lit.String(n)})"
  }

  def createBodyParameters(graphParameters: Seq[Parameter]): List[Stat] = graphParameters.map { p =>
    q"""val ${varName("var_", p.name)}: PVariable =
       body.getOrCreateVariableByName(${Lit.String(p.name)})"""
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
    val reference = value._1 match {
      case v: BooleanLiteral => Lit.Boolean(v.value)
      case v: IntegerLiteral => Lit.Int(v.value)
      case v: LongLiteral => Lit.Long(v.value)
      case v: StringLiteral => Lit.String(v.value)
    }
    Term.Apply(toTermSelect(value._1), List(Term.Name(value._1.toString)))
    q"val ${varName("var__", value._2)} = body.newConstantVariable($reference)"
  }.toList

  private def toTermSelect(value: Any): Term.Select =
    asTermSelect(value.getClass.toString.substring(6).split('.').toList)

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

  def createPParams(params: Seq[Parameter]): List[Stat] = params.toList map { p =>
    val primitiveTypeName = toImportStatement(p.typ.get.toString)
    q"""private val ${Pat.Var(Term.Name(s"p_${p.name}"))}: PParameter =
            new PParameter(${Lit.String(p.name)},
            MetaElements.NodeType(classOf[$primitiveTypeName]).toString,
            new TFInputKey.NodeTypeKey(MetaElements.NodeType(classOf[$primitiveTypeName])))"""
  }

  private def hasRefVar(v: Any): String = v match {
    case VariableReference(v) => v match {
      case TemporaryVariable(name, _) => name
      case _ => ""
    }
    case _ => ""
  }

  private def varName(prefix: String, name: String) = Pat.Var(Term.Name(prefix + name))
}
