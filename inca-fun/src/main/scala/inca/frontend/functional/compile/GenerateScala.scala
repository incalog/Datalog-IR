package inca.frontend.functional.compile

import inca.frontend.functional.syntax.*
import inca.ir.util.SourceLocation
import inca.ir.Name

class GenerateScala:
  type Code = String

  private var visited: Map[Any, Seq[Code]] = Map()

  private def createIfNeeded(a: Any)(f: => Seq[Code]): Unit = visited.get(a) match {
    case None =>
      this.visited += a -> Seq()
      val stats = f
      this.visited += a -> stats
    case Some(_) => // nothing
  }

  def generated: List[Code] = visited.values.flatten.toList

  def genFunDef(fun: FunctionDef): Unit = createIfNeeded(fun) {
    val scalaParams = fun.params.toList.map { case Param(name, typ) =>
      s"${name.name}: ${transType(typ)}"
    }.mkString(", ")
    val scalaFun = s"def ${fun.name.name}($scalaParams): ${transType(fun.outType)} = ${transExp(fun.body)}"
    Seq(scalaFun)
  }

  def genCalled(trg: Var.Target, typ: Option[Type], loc: SourceLocation): Unit = trg match {
    case fun: FunctionDef => genFunDef(fun)
    case _: Var.BuiltInFunction.type => // nothing
    case _: DataConstructor => // nothing, our ADT lowering already creates these
    case trg => throw new IllegalArgumentException(s"Unknown call target $trg")
  }

  def transType(t: Type): Code = t match {
    case TAny => "Any"
    case TNothing => "Nothing"
    case TTuple(ts) => ts.map(transType).mkString("(", ", ", ")")
    case t: TName if t.isBuiltIn => t.name.name
    case t: TName => t.target match {
      case Some(data: DataDef) => data.name.name
      case Some(t) => throw new IllegalArgumentException(s"Unknown data target $t")
      case _ => throw new IllegalArgumentException(s"Cannot compile unresolved type $t")
    }
    case TSet(ty) => s"Set${transType(ty)}"
    case TFun(from, to) => s"${from.map(transType).mkString("(", ", ", ")")} => ${transType(to)}"
  }

  def transExp(exp: Expression): Code = exp match {
    case BoolLit(b) => s"$b"
    case IntLit(i) => s"$i"
    case DoubleLit(d) => s"$d"
    case StringLit(s) => s""""$s""""
    case BinOp(e1, op, e2) => s"${transExp(e1)} $op ${transExp(e2)}"
    case Call(Var(Name("min")), Seq(), Seq(e1, e2)) => s"(${transExp(e1)}).min(${transExp(e2)})"
    case Call(Var(Name("max")), Seq(), Seq(e1, e2)) => s"(${transExp(e1)}).max(${transExp(e2)})"
    case Call(Var(Name("abs")), Seq(), Seq(e)) => s"(${transExp(e)}).abs"
    case Call(Var(Name("toString")), Seq(), Seq(e)) => s"${transExp(e)}.toString"
    case UnOp(op, e) => s"$op$e"
    case v@Var(name) =>
      v.target match
        case Some(fun: FunctionDef) => genCalled(fun, v.typ, v)
        case Some(constr: DataConstructor) => genCalled(constr, v.typ, v)
        case _ => // nothing
      name.name
    case Let(names, anno, bound, body) =>
      val scalaNames = names.map(_.name).mkString("(", ", ", ")")
      s"{val $scalaNames: ${transType(bound.typ.get)} = ${transExp(bound)}; ${transExp(body)} }"
    case If(cnd, thn, els) =>
      s"if (${transExp(cnd)}) ${transExp(thn)} else ${transExp(els)}"
    case call@Call(v@Var(name), _, args) =>
      genCalled(v.target.getOrElse(throw new IllegalArgumentException(s"Unresolved call $call")), call.typ, call)
      val argsS = args.map(a => transExp(a)).mkString(", ")
      s"${name.name}($argsS)"
    case Lambda(vs, body) =>
      val params = vs.map { case (name, ty) => s"${name.name}: ${transType(ty)}" }.mkString("(", ", ", ")")
      s"$params => ${transExp(body)}"
    case Tuple(exps) =>
      exps.map(e => transExp(e)).mkString("(", ", ", ")")
    case Match(matchee, cases) =>
      val scalaCases = cases.map {
        case (ConstructorPattern(constr, xs), e) =>
          val pVars = xs.map(x => x.name.name).mkString(", ")
          s"case ${constr.name}($pVars) => ${transExp(e)}"
      }.mkString("\n")
      s"${transExp(matchee)} match {\n$scalaCases\n}"
    case SetExp(es) =>
      val argsS = es.map(e => transExp(e)).mkString(", ")
      s"scala.Set($argsS)"
    case SetComprehension(build, predicates) =>
      val enumerators = predicates.foldLeft("") {
        case (acc, SetMember(v: Var, set, false)) if v.target.isEmpty =>
          val prefix = if (acc.isEmpty) "" else "; "
          s"$prefix${transExp(v)} <- ${transExp(set)}"
        case (acc, SetMember(tt@Tuple(ts), set, false)) if ts.forall(t => t.isInstanceOf[Var] && t.asInstanceOf[Var].target.isEmpty) =>
          val prefix = if (acc.isEmpty) "" else "; "
          s"$prefix${transExp(tt)} <- ${transExp(set)}"
        case (acc, e) =>
          acc + s"if ${transExp(e)}"
      }
      s"for ($enumerators) yield ${transExp(build)}"
    case SetMember(tup, set, neg) =>
      val member = s"${transExp(set)}.contains(${transExp(tup)})"
      if (neg)
        s"!$member"
      else
        member
    case SetFold(_, init, op: Var, set) =>
      s"${transExp(set)}.fold(${transExp(init)})(${transFoldOp(op, exp.typ)})"
  }

  def transFoldOp(op: Var, typ: Option[Type]): Code = {
    genCalled(op.target.getOrElse(throw new IllegalArgumentException(s"Unresolved fold $op")), typ, op)
    op.name.name
  }
