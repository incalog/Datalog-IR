package inca.frontend.funext

import inca.frontend.fun.Fun._
import inca.frontend.funext.desugar.{DesugarTrans, Desugarable}
import inca.util.{Gensym, Meta}

import scala.collection.mutable.ListBuffer

case class Match(matchee: Exp, cases: Seq[Case]) extends Statement {
  override def boundVars: Set[Name] = cases.flatMap(_.boundVars).toSet
  override def allVars: Map[Name, Option[TypeAnno]] = matchee.freeVars ++ cases.flatMap(_.allVars)

  override def prettyprint(implicit indent: String): String = {
    val casesS = if (cases.isEmpty) "" else
    "\n" + cases.map(_.prettyprint(indent+Meta.TAB)).mkString("\n")
    s"""${indent}${matchee.prettyprint} match {$casesS
       |${indent}}""".stripMargin
  }

}
case class Case(pattern: Pattern, body: Body) {
  def boundVars: Set[Name] = pattern.boundVars ++ body.boundVars
  def allVars: Map[Name, Option[TypeAnno]] = pattern.allVars ++ body.allVars

  def prettyprint(implicit indent: String): String =
    s"${indent}case ${pattern.prettyprint} => ${body.prettyprint}"
}

sealed trait Pattern {
  def boundVars: Set[Name]
  def allVars: Map[Name, Option[TypeAnno]]
  def prettyprint(implicit indent: String): String
}

case class NodePattern(c: TNode, bindings: Seq[PatternBinding]) extends Pattern {
  def boundVars: Set[Name] = bindings.flatMap(_.pattern.boundVars).toSet
  override def allVars: Map[Name, Option[TypeAnno]] = bindings.flatMap(_.pattern.allVars).toMap

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
  override def boundVars: Set[Name] = pats.flatMap(_.boundVars).toSet
  override def allVars: Map[Name, Option[TypeAnno]] = pats.flatMap(_.allVars).toMap
  override def prettyprint(implicit indent: String): String =
    if (pats.isEmpty)
      "()"
    else if (pats.size == 1)
      pats.head.prettyprint
    else
      pats.map(_.prettyprint).mkString("(", ", ", ")")
}

case class VarPattern(name: Name) extends Pattern {
  override def boundVars: Set[Name] = Set(name)
  override def allVars: Map[Name, Option[TypeAnno]] = Map(name -> None)
  override def prettyprint(implicit indent: String): String = name
}
case class NamedPattern(name: Name, pat: Pattern) extends Pattern {
  override def boundVars: Set[Name] = Set(name) ++ pat.boundVars
  override def allVars: Map[Name, Option[TypeAnno]] = Map(name -> None) ++ pat.allVars
  override def prettyprint(implicit indent: String): String = s"$name@${pat.prettyprint}"
}

case object WildcardPattern extends Pattern {
  override def boundVars: Set[Name] = Set()
  override def allVars: Map[Name, Option[TypeAnno]] = Map()
  override def prettyprint(implicit indent: String): String = "_"
}

case class LiteralPattern(v: Literal) extends Pattern {
  override def boundVars: Set[Name] = Set()
  override def allVars: Map[Name, Option[TypeAnno]] = Map()
  override def prettyprint(implicit indent: String): String = v.prettyprint
}



object Match extends Desugarable {
  override val desugarsTo: Seq[Desugarable] = Seq(Switch, BoolOps)

  override def trans(): DesugarTrans = new DesugarTrans {

    override def desugarStm(stm: Statement)(implicit gensym: Gensym): Seq[Statement] = stm match {
      case Match(matchee, cases) =>
        var notPatsAlternatives = Seq(Seq[Statement]())
        val bodies = cases.flatMap { cas =>
          val conds = desugarPat(desugarExp(matchee), cas.pattern)
          val casebodies = notPatsAlternatives.map( notconds =>
            Body(notconds ++ conds ++ cas.body.stmts.flatMap(desugarStm))
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