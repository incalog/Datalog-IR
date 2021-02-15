package inca.backend.ir

import inca.backend.ir.GP.{Body, Call, Comparator, Compare, Computation, Computed, Constant, Constraint, EqComparator, HasType, Link, Module, NamedLink, NeqComparator, NoPath, NotHasType, Param, Path, Pattern, Private, TAny, TAnyLinked, TList, TLiteral, TNode, TScala, Term, Type, Var, Visibility}
import truechange.JavaLitType

object Printer {

  def prettyModule(module: Module): String =
    "module " +
      module.name + "\n" +
      module.imports.mkString("\n") + "\n" +
      module.scalaContent.map(t => "`" + t.syntax + "`").mkString("\n") + "\n" +
      module.pats.map(prettyGraphPattern).mkString("\n")

  def prettyGraphPattern(gp: Pattern): String = {
    val header = prettyVis(gp.vis) + " " + gp.name + gp.params.map(prettyParam).mkString("(", ", ", ")")
    val bodies = gp.bodies.map(prettyAlternative).mkString(" {\n", "\n} or {\n", "\n}")
    header + bodies
  }

  def prettyVis(vis: Option[Visibility]): String = vis match {
    case Some(Private) => "private"
    case None => "public"
  }

  def prettyParam(param: Param): String = s"${param.name}: ${prettyType(param.typ)}"

  def prettyType(typ: Type): String = typ match {
    case TAny => "TAny"
    case TLiteral(litType) => litType match {
      case JavaLitType(cl) =>  cl.getName
      case _ => throw new UnsupportedOperationException
    }
    case TAnyLinked => "TAnyLinked"
    case TNode(name) => name
    case TScala(ty) => s"`${ty.syntax}`"
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
      s"${prettyTerm(lhs)} == count $patName(${args.map(prettyTerm).mkString(",")})"
    case GP.Evaluation(args, returnType, code) =>
      val syntax = code.syntax
      val indented = syntax.replace("\n", "\n\t\t")
      s"${prettyTerm(lhs)} == `$indented`: ${prettyType(returnType)}"
    case GP.CustomAggregation(typ, agg, patName, args, aggregatedColumn) =>
      val sargs = args.map(prettyTerm).updated(aggregatedColumn, "#").mkString(", ")
      s"${prettyTerm(lhs)} == aggregate $patName($sargs):$typ with $agg"
  }
}
