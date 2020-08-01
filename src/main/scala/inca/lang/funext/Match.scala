package inca.lang.funext

import inca.lang.fun.Fun._
import inca.lang.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.{Gensym, Meta}

import scala.collection.mutable.ListBuffer

case class Match(matchee: Exp, cases: Seq[Case]) extends Statement {
  override def usedvars: Set[Name] = matchee.usedvars ++ cases.flatMap(_.usedvars)

  override def prettyprint(implicit indent: String): String = {
    val casesS = if (cases.isEmpty) "" else
      "\n" + cases.map(_.prettyprint(indent+Meta.TAB)).mkString("\n")
    s"""${indent}${matchee.prettyprint} match {$casesS
       |${indent}}""".stripMargin
  }
}
case class Case(pattern: Pattern, body: Seq[Statement]) {
  def usedvars: Set[Name] = pattern.usedvars ++ body.flatMap(_.usedvars)

  def prettyprint(implicit indent: String): String = {
    val bodyS = if (body.isEmpty) "" else
      "\n" + body.map(_.prettyprint(indent+Meta.TAB)).mkString("\n")
    s"${indent}case ${pattern.prettyprint} => $bodyS"
  }
}

sealed trait Pattern {
  def usedvars: Set[Name]
  def prettyprint(implicit indent: String): String
}

case class NodePattern(c: TNode, bindings: Seq[PatternBinding]) extends Pattern {
  override def usedvars: Set[Name] = bindings.flatMap(_.pattern.usedvars).toSet

  override def prettyprint(implicit indent: String): String = {
    val bindingsS = if (bindings.isEmpty) "" else
      bindings.map(_.prettyprint).mkString(", ")
    s"${c.prettyprint}($bindingsS)"
  }
}
case class PatternBinding(field: Name, pattern: Pattern) extends Typeable {
  def prettyprint(implicit indent: String): String =
    s"$field = ${pattern.prettyprint}"
}

case class TuplePattern(pats: Seq[Pattern]) extends Pattern {
  override def usedvars: Set[Name] = pats.flatMap(_.usedvars).toSet
  override def prettyprint(implicit indent: String): String =
    if (pats.isEmpty)
      "()"
    else if (pats.size == 1)
      pats.head.prettyprint
    else
      pats.map(_.prettyprint).mkString("(", ", ", ")")
}

case class VarPattern(name: Name) extends Pattern {
  override def usedvars: Set[Name] = Set(name)
  override def prettyprint(implicit indent: String): String = name
}
case class NamedPattern(name: Name, pat: Pattern) extends Pattern {
  override def usedvars: Set[Name] = Set(name) ++ pat.usedvars
  override def prettyprint(implicit indent: String): String = s"$name@${pat.prettyprint}"
}

case object DefaultPattern extends Pattern {
  override def usedvars: Set[Name] = Set()
  override def prettyprint(implicit indent: String): String = "default"
}
case object WildcardPattern extends Pattern {
  override def usedvars: Set[Name] = Set()
  override def prettyprint(implicit indent: String): String = "_"
}

case class LiteralPattern(v: Literal) extends Pattern {
  override def usedvars: Set[Name] = Set()
  override def prettyprint(implicit indent: String): String = v.prettyprint
}



object Match extends Desugarable {
  override val desugarsTo: Set[Desugarable] = Set(Switch, Not)

  override def trans(): DesugarTrans = new DesugarTrans {

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case Match(matchee, cases) =>
        var notPatsAlternatives = Seq(Seq[Statement]())
        val bodies = cases.flatMap { cas =>
          val conds = desugarPat(desugarExp(matchee), cas.pattern)
          val casebodies = notPatsAlternatives.map( notconds =>
            Body(notconds ++ conds ++ cas.body.flatMap(desugarStm))
          )
          val notPatAlternatives = desugarNegatedPat(matchee, cas.pattern)
          notPatsAlternatives =
            for (notPatsAlt <- notPatsAlternatives;
                 notPatAlt <- notPatAlternatives)
              yield notPatsAlt ++ notPatAlt
          casebodies
        }
        changed(Seq(Switch(bodies)))
      case _ => super.desugarStm(stm)
    }

    def desugarPat(exp: Exp, pat: Pattern)(implicit gensym: Gensym): Seq[Statement] = pat match {
      case NodePattern(c, bindings) =>
        val result = ListBuffer[Statement]()
        val matchee: Var = exp match {
          case v: Var => v
          case _ =>
            val sym = gensym.fresh("matchee")
            result += Assign(Seq(sym), exp)
            Var(sym)
        }
        result += Assert(InstanceOf(matchee, c))
        bindings.foreach { case binding@PatternBinding(field, subpat) =>
          val typ = binding.typ.getOrElse(throw new IllegalArgumentException(s"Cannot desugar untyped pattern binding $binding"))
          result ++= desugarPat(PathAccess(matchee, NamedLink(c, field)).typed(typ), subpat)
        }
        result.toSeq
      case TuplePattern(pats) =>
        val result = ListBuffer[Statement]()
        val syms = pats.indices.map(i => gensym.fresh(s"matchee_tuple$i"))
        result += Assign(syms, exp)
        (syms zip pats).foreach { case (sym,pat) =>
          result ++= desugarPat(Var(sym), pat)
        }
        result.toSeq
      case VarPattern(name) =>
        Seq(Assign(Seq(name), exp))
      case NamedPattern(name, pat) =>
        Assign(Seq(name), exp) +: desugarPat(exp, pat)
      case WildcardPattern =>
        Seq()
      case LiteralPattern(v) =>
        Seq(Assert(Eq(exp, Constant(v))))
    }

    def desugarNegatedPat(exp: Exp, pat: Pattern)(implicit gensym: Gensym): Seq[Seq[Statement]] = pat match {
      case NodePattern(c, bindings) =>
        var ensureVar = Seq[Statement]()
        val matchee: Var = exp match {
          case v: Var => v
          case _ =>
            val sym = gensym.fresh("matchee")
            ensureVar = Seq(Assign(Seq(sym), exp))
            Var(sym)
        }
        val wrongType = ensureVar :+ Assert(NotInstanceOf(matchee, c))
        val prefix = ensureVar :+ Assert(InstanceOf(matchee, c))
        val alts = bindings.flatMap { case binding@PatternBinding(field, subpat) =>
          val typ = binding.typ.getOrElse(throw new IllegalArgumentException(s"Cannot desugar untyped pattern binding $binding"))
          val patAlts = desugarNegatedPat(PathAccess(matchee, NamedLink(c, field)).typed(typ), subpat)
          patAlts.map(prefix ++ _)
        }
        wrongType +: alts
      case TuplePattern(pats) =>
        val syms = pats.indices.map(i => gensym.fresh(s"matchee_tuple$i"))
        val bind = Assign(syms, exp)
        val alts = (syms zip pats).flatMap { case (sym,pat) =>
          desugarNegatedPat(Var(sym), pat)
        }
        alts.map(bind +: _)
      case VarPattern(name) =>
        Seq()
      case NamedPattern(_, pat) =>
        desugarNegatedPat(exp, pat)
      case WildcardPattern =>
        Seq()
      case LiteralPattern(v) =>
        Seq(Seq(Assert(Neq(exp, Constant(v)))))
    }
  }
}