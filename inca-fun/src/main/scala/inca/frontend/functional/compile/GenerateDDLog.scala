package inca.frontend.functional.compile

import inca.frontend.functional.syntax.*
import inca.ir.Name
import inca.ir.util.SourceLocation

// TODO: Here is probably a bug that prevents recursive user-defined types to be correctly boxed / unboxed
//  when passed into an aggregation
class GenerateDDLog:
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
    val ddlogParams = fun.params.toList.map { case Param(name, typ) =>
      s"${name.name}: ${transType(typ)}"
    }.mkString(", ")
    val ddlogFun = s"function ${fun.name.name}($ddlogParams): ${transType(fun.outType)} {${transExp(fun.body)}}"
    Seq(ddlogFun)
  }

  def genCalled(trg: Var.Target, typ: Option[Type], loc: SourceLocation): Unit = trg match {
    case fun: FunctionDef => genFunDef(fun)
    case _: Var.BuiltInFunction.type => // nothing
    case _: DataConstructor => // nothing, our ADT lowering already creates these
    case trg => throw new IllegalArgumentException(s"Unknown call target $trg")
  }

  def transType(t: Type): Code = t match {
    case TAny => throw new IllegalArgumentException(s"Does not support TAny")
    case TNothing =>  throw new IllegalArgumentException(s"Does not support TNothing")
    case TTuple(ts) => ts.map(transType).mkString("(", ", ", ")")
    case t: TName if t.isBuiltIn => translateBuiltInTypes(t.name.name)
    case t: TName => t.target match {
      case Some(data: DataDef) => data.name.name
      case Some(t) => throw new IllegalArgumentException(s"Unknown data target $t")
      case _ => throw new IllegalArgumentException(s"Cannot compile unresolved type $t")
    }
    case TSet(ty) => s"Set<${transType(ty)}>"
    case TFun(from, to) => s"function${from.map(transType).mkString("(", ", ", ")")}: ${transType(to)}"
  }

  def translateBuiltInTypes(t: String): String = t match {
    case "Int" => "bigint"
    case "Boolean" => "bool"
    case "String" => "string"
    case "Double"  => "double"
  }

  def transExp(exp: Expression): Code = exp match {
    case BoolLit(b) => s"$b"
    case IntLit(i) => s"$i"
    case DoubleLit(d) => s"$d"
    case StringLit(s) => s""""$s""""
    case BinOp(e1, op, e2) =>
      if (op == "++")
        s"set_union(${transExp(e1)}, ${transExp(e2)})"
      else
        s"${transExp(e1)} $op ${transExp(e2)}"
    case Call(Var(Name("min")), Seq(), Seq(e1, e2)) => s"(${transExp(e1)}).min(${transExp(e2)})"
    case Call(Var(Name("max")), Seq(), Seq(e1, e2)) =>  s"(${transExp(e1)}).max(${transExp(e2)})"
    case Call(Var(Name("abs")), Seq(), Seq(e)) => s"(if (${transExp(e)} > 0) (${transExp(e)}) else (-(${transExp(e)})))"
    case Call(Var(Name("toString")), Seq(), Seq(e)) => s"to_string(${transExp(e)})"
    case UnOp(op, e) => s"$op$e"
    case v@Var(name) =>
      v.target match
        case Some(fun: FunctionDef) => genCalled(fun, v.typ, v)
        case Some(constr: DataConstructor) => genCalled(constr, v.typ, v)
        case _ => // nothing
      name.name
    case Let(names, anno, bound, body) =>
      val scalaNames = names.map(_.name).mkString("(", ", ", ")")
      bound match {
        case SetComprehension(build, predicates) =>
          s"""${transExp(bound)}
             |var $scalaNames: ${transType(bound.typ.get)} = tmp; ${transExp(body)}""".stripMargin

        case _ => s"var $scalaNames: ${transType(bound.typ.get)} = ${transExp(bound)}; ${transExp(body)}"
        }
      // Alternative Version: s"var $scalaNames: ${transType(bound.typ.get)} = {${transExp(bound)}}; ${transExp(body)}"
    case If(cnd, thn, els) =>
      s"if (${transExp(cnd)}) {${transExp(thn)}} else {${transExp(els)}}"
    case call@Call(v@Var(name), _, args) if v.target.exists(_.isInstanceOf[DataConstructor]) =>
      val argsS = args.zipWithIndex.map { (a, i) => s".param${name.name}$i=${transExp(a)}" }.mkString(", ")
      s"${name.name}{$argsS}"
    case call@Call(v@Var(name), _, args) =>
      genCalled(v.target.getOrElse(throw new IllegalArgumentException(s"Unresolved call $call")), call.typ, call)
      val argsS = args.map(a => transExp(a)).mkString(", ")
      s"${name.name}($argsS)"
    case Lambda(vs, body) =>
      val params = vs.map { case (name, ty) => s"${name.name}: ${transType(ty)}" }.mkString("|", ", ", "|")
      s"$params ${transExp(body)}"

    case Tuple(exps) =>
      exps.map(e => transExp(e)).mkString("(", ", ", ")")
    case Match(matchee, cases) =>
      val scalaCases = cases.map {
        case (ConstructorPattern(constr, xs), e) =>
          val pVars = xs.map(x => x.name.name).mkString(", ")
          s"${constr.name}{$pVars} -> ${transExp(e)}"
      }.mkString(",\n")
      s" match (${transExp(matchee)}) {\n$scalaCases\n}"
    case SetExp(es) =>
      if (es.isEmpty) "set_empty()"
      else
        s"to_set(${es.map(e => transExp(e)).mkString("[", ", ", "]")})"
      /* Alternative Version 
      else if (es.length == 1)
        val arg = transExp(es.head)
        s"set_singleton($arg)"
      else
        es.foldLeft("set_empty()") {
          case (acc, entry) => acc + s".set_union(set_singleton(${transExp(entry)}))"
        }
      */
    case SetComprehension(build, predicates) =>
      val enumerators = predicates.foldLeft("") {
        case (acc, SetMember(v: Var, set, false)) if v.target.isEmpty =>
          acc + s"for (${transExp(v)} in ${transExp(set)}){"
        case (acc, SetMember(tt@Tuple(ts), set, false)) if ts.forall(t => t.isInstanceOf[Var] && t.asInstanceOf[Var].target.isEmpty) =>
          acc + s"for (${transExp(tt)} in ${transExp(set)}){"
        case (acc, e) =>
          acc + s"if ${transExp(e)}{"
      }
      val closingbrackets = predicates.foldLeft("") {
        case (acc, _) => acc + s"}"
      }
      //Todo freshnames für tmp
      s"""var tmp = set_empty();
         |$enumerators tmp.insert(${transExp(build)})$closingbrackets;""".stripMargin

         // Alternative Version |$enumerators tmp = set_union(tmp, set_singleton(${transExp(build)}) $closingbrackets""".stripMargin

    case SetMember(exp, set, neg) =>
      val member = s"${transExp(set)}.contains(${transExp(exp)})"
      if (neg)
        s"not $member"
      else
        member
    case SetFold(_, init, op: Var, set) =>
      set match {
        case SetComprehension(build, predicates) =>
          s"""${transExp(set)}
             |tmp.fold(${transFoldOp(op, exp.typ)}, ${transExp(init)})""".stripMargin

        case _ => s"${transExp(set)}.fold(${transFoldOp(op, exp.typ)}, ${transExp(init)})"
    }
  }

  def transFoldOp(op: Var, typ: Option[Type]): Code = {
    genCalled(op.target.getOrElse(throw new IllegalArgumentException(s"Unresolved fold $op")), typ, op)
    op.name.name
  }
