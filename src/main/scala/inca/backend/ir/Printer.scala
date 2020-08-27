package inca.backend.ir

import inca.backend.ir.GP.{Body, Call, Comparator, Compare, Computation, Computed, Constant, Constraint, EqComparator, HasType, Link, Module, NamedLink, NeqComparator, NoPath, NotHasType, Param, Path, Pattern, Private, Public, TAny, TAnyLinked, TBool, TDouble, TInt, TList, TLong, TNode, TString, Term, TypeAnno, Var, Visibility}

object Printer {

  def prettyModule(module: Module): String =
    "module " + module.name + "\n" + module.imports.mkString("\n") + module.pats.map(prettyGraphPattern).mkString("\n")

  def prettyGraphPattern(gp: Pattern): String = {
    val header = prettyVis(gp.vis) + gp.name + gp.params.map(prettyParam).mkString("(", ", ", ")")
    val bodies = gp.bodies.map(prettyAlternative).mkString(" {\n", "\n} or {\n", "\n}")
    header + bodies
  }

  def prettyVis(vis: Option[Visibility]): String =
    if (vis.isDefined) vis.get match {
      case Private => "private "
      case Public => "public "
    }
    else ""

  def prettyParam(param: Param): String = s"${param.name}: ${prettyType(param.typ)}"

  def prettyType(typ: TypeAnno): String = typ match {
    case TAny => "TAny"
    case TBool => "TBool"
    case TInt => "TInt"
    case TLong => "TLong"
    case TDouble => "TDouble"
    case TString => "TString"
    case TAnyLinked => "TAnyLinked"
    case TNode(name) => name
    case TList(ty) => s"List[${prettyType(ty)}]"
  }

  def prettyAlternative(alt: Body): String = alt.constraints.map(prettyConstraint).map("\t"+_).mkString("\n")

  def prettyConstraint(constraint: Constraint): String = constraint match {
    case Compare(comp, lhs, rhs) => prettyTerm(lhs) + " " + prettyComparator(comp) + " " + prettyTerm(rhs)
    case HasType(v, typ) => prettyType(typ) + "(" + prettyTerm(v) + ")"
    case NotHasType(v, typ) => "not " + prettyType(typ) + "(" + prettyTerm(v) + ")"
    case Path(src, srcTy, link, trg, trgTy) =>
      s"${prettyLink(link)}(${prettyTerm(src)}:${prettyType(srcTy)}, ${prettyTerm(trg)}:${prettyType(trgTy)})"
    case NoPath(t, ty, link, termIsSource) =>
      if (termIsSource)
        s"not ${prettyLink(link)}(${prettyTerm(t)}:${prettyType(ty)}, _:_)"
      else
        s"not ${prettyLink(link)}(_:_, ${prettyTerm(t)}:${prettyType(ty)})"
    case Call(name, args, isTransitive, isNeg) =>
      val neg = if (isNeg) "neg " else ""
      val trans = if (isTransitive) "+" else ""
      val call = s"$name$trans(${args.map(prettyTerm).mkString(",")})"
      s"${neg}find $call"
    case Computed(lhs, computation) =>
      prettyComputation(lhs, computation)
  }

  def prettyLink(link: Link): String = link match {
    case GP.ParentLink => "parent"
    case GP.NextLink => "next"
    case GP.SizeLink => "size"
    case NamedLink(node, field) => s"${prettyType(node)}.$field"
  }

  def prettyTerm(value: Term): String = value match {
    case Var(name) => name
    case Constant(lit) => lit match {
      case GP.IntLiteral(v) => v.toString
      case GP.LongLiteral(v) => v.toString
      case GP.DoubleLiteral(v) => v.toString
      case GP.StringLiteral(v) => v
      case GP.BooleanLiteral(v) => v.toString
    }
  }

  def prettyComparator(comp: Comparator): String = comp match {
    case EqComparator => "=="
    case NeqComparator => "!="
  }

  def prettyComputation(lhs: Term, computation: Computation): String = computation match {
    case GP.CountAggregation(patName, args) =>
      s"${prettyTerm(lhs)} = count $patName(${args.map(prettyTerm).mkString(",")})"
    case GP.Evaluation(args, _, code) =>
      s"${prettyTerm(lhs)} = eval(($code)(${args.map(a => prettyTerm(a._1)).mkString(", ")}))"
    case GP.CustomAggregation(typ, initOp, joinOp, inverseOp, patName, args, aggregatedColumn) =>
      val sargs = args.map(prettyTerm).updated(aggregatedColumn, "#").mkString(", ")
      s"${prettyTerm(lhs)} = aggregate $patName($sargs) with $initOp, $joinOp, $inverseOp"
  }
}
