package inca.backend.ir.util.printer

import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.{Atom, Body, Call, Comparator, Compare, Computation, Computed, Constant, EqComparator, ExtensionalCall, HasType, Link, Module, NamedLink, NeqComparator, NoPath, NotHasType, Param, Path, Pattern, Private, TAny, TAnyLinked, TData, TList, TLiteral, TNode, TScala, Term, Type, Undef, Var, Visibility}
import truechange.JavaLitType

object DatalogPrinter {

  def prettyModule(module: Module)(implicit verbose: Boolean): String =
    "module " +
      module.name + "\n" +
      module.imports.mkString("\n") + "\n" +
      module.scalaContent.map(t => "`" + t.syntax + "`").mkString("\n") + "\n" +
      module.pats.map(prettyPattern).mkString("\n\n")

  def prettyPattern(pat: Pattern)(implicit verbose: Boolean): String = {
    val decl = "decl " + prettyVis(pat.vis) + pat.name + pat.params.map(prettyParam).mkString("(", ", ", ")")
    val head = pat.name + pat.params.map(p => p.name).mkString("(", ", ", ")")
    val rules = pat.bodies.map(b => s"$head :- ${prettyBody(b)}.")
    s"$decl\n${rules.mkString("\n")}"
  }

  def prettyVis(vis: Option[Visibility])(implicit verbose: Boolean): String = vis match {
    case Some(Private) => "private "
    case None => ""
  }

  def prettyParam(param: Param)(implicit verbose: Boolean): String = s"${param.name}: ${prettyType(param.typ)}"

  def prettyType(typ: Type)(implicit verbose: Boolean): String = typ match {
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

  def prettyBody(alt: Body)(implicit verbose: Boolean): String = {
    val atoms = alt.atoms.map(prettyAtom)
    atoms.head + (if(atoms.size > 1) ",\n" else "") + atoms.tail.map("\t"+_).mkString(",\n")
  }

  def prettyAtom(atom: Atom)(implicit verbose: Boolean): String = atom match {
    case Compare(comp, lhs, rhs) => prettyTerm(lhs) + " " + prettyComparator(comp) + " " + prettyTerm(rhs)
    case HasType(v, typ) => "type " + prettyType(typ) + "(" + prettyTerm(v) + ")"
    case NotHasType(v, typ) => "!type" + prettyType(typ) + "(" + prettyTerm(v) + ")"
    case Path(src, srcTy, link, trg, trgTy) =>
      if (verbose)
        s"${prettyLink(link)}(${prettyTerm(src)}:${prettyType(srcTy)}, ${prettyTerm(trg)}:${prettyType(trgTy)})"
      else
        s"${prettyLink(link)}(${prettyTerm(src)}, ${prettyTerm(trg)})"
    case NoPath(t, ty, link, termIsSource) =>
      if (termIsSource)
        if (verbose)
          s"!${prettyLink(link)}(${prettyTerm(t)}:${prettyType(ty)}, _:_)"
        else
          s"!${prettyLink(link)}(${prettyTerm(t)}, _)"
      else
        if (verbose)
          s"!${prettyLink(link)}(_, ${prettyTerm(t)})"
        else
          s"!${prettyLink(link)}(_:_, ${prettyTerm(t)}:${prettyType(ty)})"
    case Call(name, args, isTransitive, isNeg) =>
      val neg = if (isNeg) "!" else ""
      val trans = if (isTransitive) "+" else ""
      val call = s"$name$trans(${args.map(prettyTerm).mkString(",")})"
      s"$neg$call"
    case Computed(lhs, computation) =>
      prettyComputation(lhs, computation)
    case ExtensionalCall(name, args, isNeg) =>
      val neg = if (isNeg) "!" else ""
      val call = s"$name(${args.map(prettyTerm).mkString(",")})"
      s"ext $neg$call"
    case Undef(t) =>
      s"!${prettyTerm(t)}"
  }

  def prettyLink(link: Link)(implicit verbose: Boolean): String = link match {
    case Datalog.ParentLink => "parent"
    case Datalog.NextLink => "next"
    case Datalog.SizeLink => "size"
    case NamedLink(node, field) => s"${prettyType(node)}.$field"
  }

  def prettyTerm(value: Term)(implicit verbose: Boolean): String = value match {
    case Var(name) => name
    case Constant(lit) => lit match {
      case Datalog.IntLiteral(v) => v.toString
      case Datalog.LongLiteral(v) => v.toString
      case Datalog.DoubleLiteral(v) => v.toString
      case Datalog.StringLiteral(v) => v
      case Datalog.BooleanLiteral(v) => v.toString
    }
  }

  def prettyComparator(comp: Comparator)(implicit verbose: Boolean): String = comp match {
    case EqComparator => "="
    case NeqComparator => "!="
  }

  def prettyComputation(lhs: Term, computation: Computation)(implicit verbose: Boolean): String = computation match {
    case Datalog.CountAggregation(patName, args) =>
      s"${prettyTerm(lhs)} = count $patName(${args.map(prettyTerm).mkString(",")})"
    case Datalog.Evaluation(args, returnType, code) =>
      val indented = if (verbose) {
        s"${code.syntax.replace("\n", "\n\t\t")}: ${prettyType(returnType)}"
      } else {
        val typelessParams = code.tree.params.map(_.name.syntax).mkString("(", ", ", ")")
        s"$typelessParams => ${code.tree.body.syntax.replace("\n", "\n\t\t")}"
      }
      val argsS = args.map(a => prettyTerm(a._1)).mkString(", ")
      s"${prettyTerm(lhs)} = `$indented`($argsS)"
    case Datalog.CustomAggregation(typ, desc, agg, patName, args, aggregatedColumn) =>
      val sargs = args.map(prettyTerm).updated(aggregatedColumn, "#").mkString(", ")
      s"${prettyTerm(lhs)} = aggregate $patName($sargs):$typ with ${desc.getOrElse(agg.toString)}"
  }
}
