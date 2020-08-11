package inca.backend.ir

import inca.backend.ir.GP.{Body, Call, Comparator, Compare, Computation, Computed, Constant, Constraint, EqComparator, HasType, Module, NamedLink, NeqComparator, NextLink, Param, ParentLink, Path, Pattern, Private, Public, SizeLink, TAnyLinked, TBool, TDouble, TInt, TList, TLong, TNode, TString, Term, TypeAnno, Var, Visibility}

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

  def prettyParam(param: Param): String = param.name + (if (param.typ.isDefined) ": " + prettyType(param.typ.get) else "")

  def prettyType(typ: TypeAnno): String = typ match {
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
    case Path(src, trg, link, ty) => link match {
      case NamedLink(node, fld) =>
        prettyType(node) + "." + fld + "(" + prettyTerm(src) + ", " + prettyTerm(trg) + "):" + prettyType(ty)
      case ParentLink =>
        "parent(" + prettyTerm(src) + ", " + prettyTerm(trg) + "):" + prettyType(ty)
      case NextLink =>
        "next(" + prettyTerm(src) + ", " + prettyTerm(trg) + "):" + prettyType(ty)
      case SizeLink =>
        "size(" + prettyTerm(src) + ", " + prettyTerm(trg) + "):" + prettyType(ty)
    }
    case Call(name, args, isTransitive, isNeg) =>
      val neg = if (isNeg) "neg " else ""
      val trans = if (isTransitive) "+" else ""
      val call = s"$name$trans(${args.map(prettyTerm).mkString(",")})"
      s"${neg}find $call"
    case Computed(resultVar, computation) =>
      prettyComputation(resultVar, computation)
  }

  def prettyTerm(value: Term): String = value match {
    case Var(name) => name
    case Constant(lit) => lit.toString
  }

  def prettyComparator(comp: Comparator): String = comp match {
    case EqComparator => "=="
    case NeqComparator => "!="
  }

  def prettyComputation(resultVar: Var, computation: Computation): String = computation match {
    case GP.CountAggregation(name, args) =>
      s"${prettyTerm(resultVar)} = count $name(${args.map(prettyTerm).mkString(",")})"
    case GP.Evaluation(_, _, code) =>
      s"${prettyTerm(resultVar)} = eval($code)"
    case GP.LatticeAggregation() => ???
  }
}
