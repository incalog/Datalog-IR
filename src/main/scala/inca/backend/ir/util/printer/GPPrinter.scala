package inca.backend.ir.util.printer

import inca.backend.hints.MagicSetHints
import inca.backend.ir.Datalog
import inca.backend.ir.Datalog.{Atom, Body, Call, Comparator, Compare, Computation, Computed, Constant, EqComparator, ExtensionalCall, HasType, Link, Module, NamedLink, NeqComparator, NoPath, NotHasType, Param, Path, Pattern, Private, TAny, TAnyLinked, TData, TList, TLiteral, TNode, TScala, Term, Type, Undef, Var, Visibility}
import inca.backend.optimize.EvalFusion
import truechange.JavaLitType

object GPPrinter {

  private def inOwnLine(s: String): String =
    if (s.isEmpty)
      ""
    else
      s"$s\n"

  def prettyModule(module: Module): String =
    "module " +
      module.name + "\n" +
      inOwnLine(module.imports.mkString("\n")) +
      inOwnLine(module.scalaContent.map(t => "`" + t.syntax + "`").mkString("\n")) +
      module.pats.map(prettyPattern).mkString("\n")

  def prettyPattern(gp: Pattern): String = {
    val header = prettyVis(gp.vis) + gp.name + gp.params.map(prettyParam).mkString("(", ", ", ")")
    val bodies = gp.bodies.map(prettyBody).mkString(" {\n", "\n} or {\n", "\n}")
    header + " " + gp.hints.get(MagicSetHints.DemandPatternsKey).map(h => h.asInstanceOf[MagicSetHints.DemandPatterns].adorn).getOrElse("") +  bodies
  }

  def prettyVis(vis: Option[Visibility]): String = vis match {
    case Some(Private) => "private "
    case None => ""
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
    case TScala(ty) => ty.syntax
    case TList(ty) => s"List[${prettyType(ty)}]"
  }

  def prettyBody(alt: Body): String = alt.atoms.map(prettyAtom).map("\t"+_).mkString("\n")

  def prettyAtom(atom: Atom): String = (atom match {
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
  })// + atom.hints

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
      if (args.forall(_._1.isInstanceOf[Var])) {
        val params = code.tree.params.map(_.name.value)
        val scalaArgs = args.map(a => meta.Term.Name(a._1.asInstanceOf[Var].name))
        val codeS = EvalFusion.scalaSubst(code.tree.body, Map() ++ params.zip(scalaArgs)).syntax.replace("\n", "\n\t\t")
        s"${prettyTerm(lhs)} == `(${code.tree}: ${returnType.asScala})(${args.map(_._1.asInstanceOf[Var].name).mkString(", ")})`"
        //s"${prettyTerm(lhs)} == `$codeS`"
      } else {
        val codeS = code.syntax.replace("\n", "\n\t\t")
        val argsS = args.map(a => prettyTerm(a._1)).mkString(", ")
        s"${prettyTerm(lhs)} == `$codeS`($argsS)"
      }
    case Datalog.CustomAggregation(typ, desc, agg, patName, args, aggregatedColumn) =>
      val sargs = args.map(prettyTerm).updated(aggregatedColumn, "#").mkString(", ")
      s"${prettyTerm(lhs)} == aggregate $patName($sargs):$typ with ${desc.getOrElse(agg.toString)}"
  }
}
