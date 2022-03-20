package inca.backend.ir

import inca.backend.ir.Datalog.Atom
import inca.backend.ir.Datalog.Body
import inca.backend.ir.Datalog.Call
import inca.backend.ir.Datalog.Comparator
import inca.backend.ir.Datalog.Compare
import inca.backend.ir.Datalog.Computation
import inca.backend.ir.Datalog.Computed
import inca.backend.ir.Datalog.Constant
import inca.backend.ir.Datalog.EqComparator
import inca.backend.ir.Datalog.ExtensionalCall
import inca.backend.ir.Datalog.HasType
import inca.backend.ir.Datalog.Link
import inca.backend.ir.Datalog.Module
import inca.backend.ir.Datalog.NamedLink
import inca.backend.ir.Datalog.NeqComparator
import inca.backend.ir.Datalog.NoPath
import inca.backend.ir.Datalog.NotHasType
import inca.backend.ir.Datalog.Param
import inca.backend.ir.Datalog.Path
import inca.backend.ir.Datalog.Pattern
import inca.backend.ir.Datalog.Private
import inca.backend.ir.Datalog.TAny
import inca.backend.ir.Datalog.TAnyLinked
import inca.backend.ir.Datalog.TData
import inca.backend.ir.Datalog.TList
import inca.backend.ir.Datalog.TLiteral
import inca.backend.ir.Datalog.TNode
import inca.backend.ir.Datalog.TScala
import inca.backend.ir.Datalog.Term
import inca.backend.ir.Datalog.Type
import inca.backend.ir.Datalog.Undef
import inca.backend.ir.Datalog.Var
import inca.backend.ir.Datalog.Visibility
import truechange.JavaLitType

object DatalogPrinter {

  def prettyModule(module: Module)(implicit verbose: Boolean): String =
    "module " +
      module.name + "\n" +
      module.imports.mkString("\n") + "\n" +
      module.scalaContent.map(t => "`" + t.syntax + "`").mkString("\n") + "\n" +
      module.pats.map(prettyPattern).mkString("\n\n")

  def prettyPattern(pat: Pattern)(implicit verbose: Boolean): String = {
    val decl =
      "decl " + prettyVis(pat.vis) + pat.name + pat.params.map(prettyParam).mkString("(", ", ", ")")
    val head = pat.name + pat.params.map(p => p.name).mkString("(", ", ", ")")
    val rules = pat.bodies.map(b => s"$head :- ${prettyBody(b)}.")
    s"$decl\n${rules.mkString("\n")}"
  }

  def prettyVis(vis: Option[Visibility]): String = vis match {
    case Some(Private) => "private "
    case None => ""
  }

  def prettyParam(param: Param): String = s"${param.name}: ${prettyType(param.typ)}"

  def prettyType(typ: Type): String = typ match {
    case TAny => "TAny"
    case TLiteral(litType) =>
      litType match {
        case JavaLitType(cl) => cl.getName
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
    atoms.head + (if (atoms.size > 1) ",\n" else "") + atoms.tail.map("\t" + _).mkString(",\n")
  }

  def prettyAtom(atom: Atom)(implicit verbose: Boolean): String = atom match {
    case Compare(comp, lhs, rhs) =>
      prettyTerm(lhs) + " " + prettyComparator(comp) + " " + prettyTerm(rhs)
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
      else if (verbose)
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

  def prettyLink(link: Link): String = link match {
    case Datalog.ParentLink => "parent"
    case Datalog.NextLink => "next"
    case Datalog.SizeLink => "size"
    case NamedLink(node, field) => s"${prettyType(node)}.$field"
  }

  def prettyTerm(value: Term): String = value match {
    case Var(name) => name
    case Constant(lit) =>
      lit match {
        case Datalog.IntLiteral(v) => v.toString
        case Datalog.LongLiteral(v) => v.toString
        case Datalog.DoubleLiteral(v) => v.toString
        case Datalog.StringLiteral(v) => v
        case Datalog.BooleanLiteral(v) => v.toString
      }
  }

  def prettyComparator(comp: Comparator): String = comp match {
    case EqComparator => "="
    case NeqComparator => "!="
  }

  def prettyComputation(lhs: Term, computation: Computation)(implicit verbose: Boolean): String =
    computation match {
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
