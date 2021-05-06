package inca.backend.ir

import inca.backend.ir.Datalog.{Body, Call, Comparator, Compare, Computation, Computed, Constant, Atom, EqComparator, ExtensionalCall, HasType, Link, Module, NamedLink, NeqComparator, NoPath, NotHasType, Param, Path, Pattern, Private, TAny, TAnyLinked, TData, TList, TLiteral, TNode, TScala, Term, Type, Undef, Var, Visibility}
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
    case TData(name) => name
    case TAnyLinked => "TAnyLinked"
    case TNode(name) => name
    case TScala(ty) => s"`${ty.syntax}`"
    case TList(ty) => s"List[${prettyType(ty)}]"
  }

  def prettyAlternative(alt: Body): String = alt.atoms.map(prettyAtom).map("\t"+_).mkString("\n")

  def prettyAtom(atom: Atom): String = atom match {
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
    case ExtensionalCall(name, args, isNeg) =>
      val neg = if (isNeg) "neg " else ""
      val call = s"$name(${args.map(prettyTerm).mkString(",")})"
      s"${neg}extensional find $call"
    case Undef(t) =>
      s"undef ${prettyTerm(t)}"
  }

  def prettyLink(link: Link): String = link match {
    case Datalog.ParentLink => "parent"
    case Datalog.NextLink => "next"
    case Datalog.SizeLink => "size"
    case NamedLink(node, field) => s"${prettyType(node)}.$field"
  }

  def prettyTerm(value: Term): String = value match {
    case Var(name) => name
    case Constant(lit) => lit match {
      case Datalog.IntLiteral(v) => v.toString
      case Datalog.LongLiteral(v) => v.toString
      case Datalog.DoubleLiteral(v) => v.toString
      case Datalog.StringLiteral(v) => v
      case Datalog.BooleanLiteral(v) => v.toString
    }
  }

  def prettyComparator(comp: Comparator): String = comp match {
    case EqComparator => "=="
    case NeqComparator => "!="
  }

  def prettyComputation(lhs: Term, computation: Computation): String = computation match {
    case Datalog.CountAggregation(patName, args) =>
      s"${prettyTerm(lhs)} == count $patName(${args.map(prettyTerm).mkString(",")})"
    case Datalog.Evaluation(args, returnType, code) =>
      val indented = code.syntax.replace("\n", "\n\t\t")
      val argsS = args.map(a => prettyTerm(a._1)).mkString(", ")
      s"${prettyTerm(lhs)} == `$indented`($argsS): ${prettyType(returnType)}"
    case Datalog.CustomAggregation(typ, desc, agg, patName, args, aggregatedColumn) =>
      val sargs = args.map(prettyTerm).updated(aggregatedColumn, "#").mkString(", ")
      s"${prettyTerm(lhs)} == aggregate $patName($sargs):$typ with ${desc.getOrElse(agg.toString)}"
  }
}
