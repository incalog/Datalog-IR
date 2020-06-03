package org.inca.print

import org.inca.lang.GraphPatternLang._
import org.inca.meta.MetaElements.DefinedNodeLink

object GraphPatternLangPrinter {

  def prettyModule(module: Module): String =
    "module " + module.name + "\n" + module.imports.mkString("\n") + module.pats.map(prettyGraphPattern).mkString("\n")

  def prettyGraphPattern(gp: GraphPattern): String = {
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

  def prettyType(typ: Type): String = typ match {
    case TNodeType(wrapped) => wrapped.cls.getName
    case TBool => "TBool"
    case TInt => "TInt"
    case TLong => "TLong"
    case TDouble => "TDouble"
    case TString => "TString"
  }

  def prettyAlternative(alt: Alternative): String = alt.constraints.map(prettyConstraint).map("\t"+_).mkString("\n")

  def prettyConstraint(constraint: Constraint): String = constraint match {
    case Compare(comp, lhs, rhs) => prettyValue(lhs) + " " + prettyComparator(comp) + " " + prettyValue(rhs)
    case Concept(v, typ) => prettyType(typ) + "(" + prettyValue(v) + ")"
    case Path(src, trg, link, typ) =>
      val isDefined = if (link.isInstanceOf[DefinedNodeLink]) "_isDefined" else ""
      prettyType(typ) + "." + link.fld.getName + isDefined + "(" + prettyValue(src) + ", " + prettyValue(trg) + ")"
    case Composition(call, neg) => (if(neg) "neg " else "") + "find " + prettyPatternCall(call)
  }

  def prettyPatternCall(call: PatternCall): String =
    call.name + (if (call.transitive) "+" else "") + call.args.map(prettyValue).mkString("(", ", ", ")")

  def prettyValue(value: Value): String = value match {
    case Var(name) => name
    case Constant(lit) => lit.toString
  }

  def prettyComparator(comp: Comparator): String = comp match {
    case EqComparator => "=="
    case NeqComparator => "!="
  }
}
