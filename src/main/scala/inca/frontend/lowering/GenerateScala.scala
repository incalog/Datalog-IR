package inca.frontend.lowering

import inca.compiler.SourceLocation
import inca.frontend.core
import inca.frontend.core._
import inca.runtime.aggregate.{Aggregation, AggregatorAssocComm}
import inca.runtime.data.DataURI
import inca.util.Meta.{symbolOf, typeOf}

import scala.meta.{Type => MetaType, _}

class GenerateScala {
  private var visited: Map[Any, Seq[meta.Stat]] = Map()
  private def createIfNeeded(a: Any)(f: => Seq[meta.Stat]): Unit = visited.get(a) match {
    case None =>
      this.visited += a -> Seq()
      val stats = f
      this.visited += a -> stats
    case Some(_) => // nothing
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
    val scalaParams = fun.params.toList.map { case Param(name, typ) =>
      param"${Term.Name(name.name)}: ${transType(typ)}"
    }
    val scalaFun = q"def ${Term.Name(fun.name.name)}(..$scalaParams): ${transType(fun.outType)} = ${transExp(fun.body.ensureCore)}"
    Seq(scalaFun)
  }

  def genCalled(trg: Call.Target, typ: Option[Type], loc: SourceLocation): Unit = trg match {
    case fun: FunctionDef =>
      genFunDef(fun)
    case _: DataConstructor =>
      val data = typ.getOrElse(throw new IllegalArgumentException(s"Untyped call $loc")).asInstanceOf[TData]
        .target.getOrElse(throw new IllegalArgumentException(s"Unresolved data type ${typ.get}")).asInstanceOf[DataDef]
      genDataDef(data)
    case trg =>
      throw new IllegalArgumentException(s"Unknown call target $trg")
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
    case call@Call(name, args, transitive) if !transitive =>
      genCalled(call.target.getOrElse(throw new IllegalArgumentException(s"Unresoved call $call")), call.typ, call)
      q"${Term.Name(name.name)}(..${args.map(a => transExp(a.ensureCore)).toList})"
    case Tuple(exps) =>
      q"(..${exps.map(e => transExp(e.ensureCore)).toList})"
    case Match(matchee, cases) =>
      val scalaCases = cases.toList.map {
        case (ConstructorPattern(constr, xs), e) =>
          p"case ${Pat.Extract(Term.Name(constr.name), xs.toList.map(x => Pat.Var(Term.Name(x.name))))} => ${transExp(e.ensureCore)}"
        case (NonePattern(), e) =>
          p"case scala.None => ${transExp(e.ensureCore)}"
        case (SomePattern(x), e) =>
          p"case scala.Some(${Pat.Var(Term.Name(x.name))}) => ${transExp(e.ensureCore)}"
      }
      q"${transExp(matchee.ensureCore)} match {..case $scalaCases}"
    case BaseLit(code) =>
      code.tree
    case BaseApply(fun, args) =>
      q"${fun.tree}(..${args.toList.map(e => transExp(e.ensureCore))})"
    case BaseApplyInfix(left, op, right) =>
      q"${transExp(left.ensureCore)} ${op.tree} ${transExp(right.ensureCore)}"
    case NoneExp() =>
      q"scala.None"
    case SomeExp(e) =>
      q"scala.Some(${transExp(e.ensureCore)})"
    case SetExp(es) =>
      q"scala.Set(..${es.toList.map(e => transExp(e.ensureCore))})"
    case SetComprehension(build, predicates) =>
      val enumerators = predicates.toList.map {
        case SetMember(v: Var, set, false) if v.target.isEmpty =>
          enumerator"${Pat.Var(Term.Name(v.name.name))} <- ${transExp(set.ensureCore)}"
        case SetMember(Tuple(ts), set, false) if ts.forall(t => t.isInstanceOf[Var] && t.asInstanceOf[Var].target.isEmpty) =>
          enumerator"${Pat.Tuple(ts.toList.map(t => Pat.Var(Term.Name(t.asInstanceOf[Var].name.name))))} <- ${transExp(set.ensureCore)}"
        case e =>
          enumerator"if ${transExp(e.ensureCore)}"
      }
      q"for(..$enumerators) yield ${transExp(build.ensureCore)}"
    case SetMember(tup, set, neg) =>
      val member = q"${transExp(set.ensureCore)}.contains(${transExp(tup.ensureCore)})"
      if (neg)
        Term.ApplyUnary(Term.Name("!"), member)
      else
        member
    case SetFold(_, init, op, set) =>
      q"${transExp(set.ensureCore)}.fold(${transExp(init.ensureCore)})(${transFoldOp(op, exp.typ)})"
  }

  def transFoldOp(op: FoldOp, typ: Option[Type]): meta.Term = {
    genCalled(op.target.getOrElse(throw new IllegalArgumentException(s"Unresoved fold $op")), typ, op)
    Term.Name(op.name.name)
  }

  def genAggregation(name: String, init: Expression, op: FoldOp, typ: Type): meta.Term = {
    val scalaInit = transExp(init.ensureCore)
    val scalaOp = transFoldOp(op, Some(typ))
    val scalaTy = transType(typ)

    val tyAggregation = typeOf[Aggregation[_]]
    val initAggregation = init"${MetaType.Apply(tyAggregation, List(scalaTy))}()"

    // TODO extract assoc and commu from annotation or verify it
    q"""
     new $initAggregation {
       override val name = $name
       override def init: $scalaTy = $scalaInit
       override def join(v1: $scalaTy, v2: $scalaTy): $scalaTy = $scalaOp(v1, v2)
       override val isAssociative = true
       override val isCommutative = true
     }"""
  }
}
