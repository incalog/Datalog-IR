package inca.frontend.lowering

import inca.frontend.core
import inca.frontend.core._
import inca.runtime.data.DataURI
import inca.util.Meta.symbolOf

import scala.meta.{Type => MetaType, _}

class GenerateScala {
  private var visited: Map[Any, Seq[meta.Stat]] = Map()
  private def createIfNeeded(a: Any)(f: => Seq[meta.Stat]): Unit = visited.get(a) match {
    case Some(_) => // nothing
    case None => visited += a -> f
  }

  def generated: List[meta.Stat] = visited.values.flatten.toList

  private val oDataURI = symbolOf(DataURI)

  def genDataDef(data: DataDef): Unit = createIfNeeded(data) {
    val dataTyp = MetaType.Name(data.name.name)
    val typ = q"sealed trait $dataTyp"
    val constrs = data.constrs.map {
      case DataConstructor(core.Name(name), paramTypes) =>
        val scalaParamTypes = paramTypes.map(transType)
        val params = scalaParamTypes.zipWithIndex.map { case (pt, ix) =>
          param"val ${Term.Name("_" + ix)}: $pt"
        }.toList
        val children = scalaParamTypes.zipWithIndex.map { case (_, ix) =>
          q"${Lit.String("_" + ix)} -> ${Term.Name("_" + ix)}"
        }.toList
        val terms = scalaParamTypes.zipWithIndex.map { case (_, ix) =>
          Term.Name("_" + ix)
        }.toList
        val makeChildren = scalaParamTypes.zipWithIndex.map { case (pt, ix) =>
          q"children($ix).asInstanceOf[$pt]"
        }.toList
        q"""case class ${MetaType.Name(name)}(..$params) extends {} with $dataTyp() with truediff.GenericDiffable() { this =>
              this.withURI($oDataURI($name, ..$terms))

              override def name: String = $name
              override def children: Seq[(String, Any)] = Seq(..$children)
              override def make(children: Seq[Any]): ${MetaType.Name(name)} = ${Term.Name(name)}(..$makeChildren)
            }
           """
    }
    typ +: constrs
  }

  def genFunDef(fun: FunctionDef): Unit = createIfNeeded(fun) {
    ???
  }

  def transType(t: Type): MetaType = t match {
    case TAny =>  t.asScala// nothing
    case TNothing =>  t.asScala// nothing
    case TTuple(ts) => t"(..${ts.toList.map(transType)})"
    case d: TData => d.target match {
      case Some(data: DataDef) =>
        genDataDef(data)
        MetaType.Name(data.name.name)
      case Some(t) => throw new IllegalArgumentException(s"Unknown data target $t")
      case _ => throw new IllegalArgumentException(s"Cannot compile unresolved type $d")
    }
    case TScala(t) => t.tree
    case TOption(ty) => t"scala.Option[${transType(ty)}]"
    case TSet(ty) => t"scala.Set[${transType(ty)}]"
  }

  def transExp(exp: CoreExpression): meta.Term = exp match {
    case Var(name) =>
      Term.Name(name.name)
    case Let(names, anno, bound, body) =>
      val scalaNames = names.map(n => Pat.Var(Term.Name(n.name))).toList
      q"{val (..$scalaNames): ${transType(exp.typ.get)} = ${transExp(bound.ensureCore)}; ${transExp(body.ensureCore)} }"
    case If(cnd, thn, els) =>
      q"if (${transExp(cnd.ensureCore)}) ${transExp(thn.ensureCore)} else ${transExp(els.ensureCore)}"
    case call@Call(name, args, transitive) =>
      call.target match {
        case Some(fun: FunctionDef) =>
          genFunDef(fun)
        case Some(_: DataConstructor) =>
          val data = call.typ.get.asInstanceOf[TData].target.asInstanceOf[DataDef]
          genDataDef(data)
        case None =>
          throw new IllegalArgumentException(s"Unresolved call")
      }
      q"${name.name}(..${args.map(a => transExp(a.ensureCore)).toList})"
    case Tuple(exps) =>
      q"(..${exps.map(e => transExp(e.ensureCore)).toList})"
    case Match(matchee, cases) => ???
    case BaseLit(code) => ???
    case BaseApply(fun, args) => ???
    case BaseApplyInfix(left, op, right) => ???
    case NoneExp() => ???
    case SomeExp(e) => ???
    case SetExp(es) => ???
    case SetComprehension(build, predicates) => ???
    case SetMember(tup, set, neg) => ???
    case SetFold(anno, init, op, set) => ???
  }

  def genAggregation(init: Expression, op: FoldOp): meta.Term =
    ???
}
