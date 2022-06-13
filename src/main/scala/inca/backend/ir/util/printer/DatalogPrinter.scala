package inca.backend.ir.util.printer

import inca.backend.hints.DebugHints
import inca.backend.hints.DebugHints.SourceConstruct
import inca.backend.hints.Hints
import inca.backend.ir.DatalogGeneric
import inca.backend.optimize.EvalFusion
import truechange.JavaLitType

class DatalogPrinter[D <: DatalogGeneric](val datalog: D) {
  import datalog._

  val PRINT_SOURCE_CONSTRUCT = true

  def prettySourceConstruct(hinted: Hints, sep: String = ""): String =
    if (PRINT_SOURCE_CONSTRUCT) {
      hinted.hints.get(DebugHints.SourceConstruct.key) match {
        case Some(SourceConstruct(constr)) =>
          val constrStr = constr.toString.replaceAll("\\s+", " ")
          s"$sep @[$constrStr]"
        case _ => ""
      }
    } else {
      ""
    }

  def prettyModule(module: Module): String =
    "module " +
      module.name + "\n" +
      module.imports.mkString("\n") + "\n" +
      module.scalaContent.map(t => "`" + t.syntax + "`").mkString("\n") + "\n" +
      module.pats.map(prettyGraphPattern).mkString("\n")

  def prettyGraphPattern(gp: Pattern): String = {
    val header =
      prettyVis(gp.vis) + " " + gp.name + gp.params.map(prettyParam).mkString("(", ", ", ")")
    val bodies = gp.bodies.map(prettyAlternative).mkString(" {\n", "\n} or {\n", "\n}")
    header + bodies + prettySourceConstruct(gp)
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
    case TScala(ty) => ty.syntax
    case TList(ty) => s"List[${prettyType(ty)}]"
  }

  def prettyAlternative(alt: Body): String = alt.atoms.map(prettyAtom).map("\t" + _).mkString("\n")

  def prettyAtom(atom: Atom): String = (atom match {
    case Compare(comp, lhs, rhs) =>
      prettyTerm(lhs) + " " + prettyComparator(comp) + " " + prettyTerm(rhs)
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
  }) + prettySourceConstruct(atom)

  def prettyLink(link: Link): String = link match {
    case datalog.ParentLink => "parent"
    case datalog.NextLink => "next"
    case datalog.SizeLink => "size"
    case NamedLink(node, field) => s"${prettyType(node)}.$field"
  }

  def prettyTerm(value: Term): String = value match {
    case Var(name) => name
    case Constant(lit) => lit match {
      case datalog.base.IntLiteral(v) => v.toString
      case datalog.base.LongLiteral(v) => v.toString
      case datalog.base.DoubleLiteral(v) => v.toString
      case datalog.base.StringLiteral(v) => s""""$v""""
      case datalog.base.BooleanLiteral(v) => v.toString
    }
  }

  def prettyComparator(comp: Comparator): String = comp match {
    case EqComparator => "=="
    case NeqComparator => "!="
  }

  def prettyComputation(lhs: Term, computation: Computation): String = computation match {
    case datalog.CountAggregation(patName, args) =>
      s"${prettyTerm(lhs)} == count $patName(${args.map(prettyTerm).mkString(",")})"
    case datalog.Evaluation(args, _, code) =>
      if (args.forall(_._1.isInstanceOf[Var])) {
        val params = code.tree.params.map(_.name.value)
        val scalaArgs = args.map(a => meta.Term.Name(a._1.asInstanceOf[Var].name))
        val codeS = EvalFusion.scalaSubst(
          code.tree.body,
          Map() ++ params.zip(scalaArgs)
        ).syntax.replace("\n", "\n\t\t")
        s"${prettyTerm(lhs)} == `$codeS`"
      } else {
        val codeS = code.syntax.replace("\n", "\n\t\t")
        val argsS = args.map(a => prettyTerm(a._1)).mkString(", ")
        s"${prettyTerm(lhs)} == `$codeS`($argsS)"
      }
    case datalog.CustomAggregation(typ, desc, agg, patName, args, aggregatedColumn) =>
      val sargs = args.map(prettyTerm).updated(aggregatedColumn, "#").mkString(", ")
      s"${prettyTerm(lhs)} == aggregate $patName($sargs):$typ with ${desc.getOrElse(agg.toString)}"
  }
}
