package inca.frontend.extensions

import inca.frontend.Frontend
import inca.frontend.core._
import inca.frontend.desugar.{DesugarTrans, Desugarable}
import inca.frontend.parser.ParserUtils.{nl_!, sp}
import inca.frontend.parser.SourceLocation
import inca.frontend.typechecker.{NoYield, StmType, Typeable}
import inca.util.{Gensym, Meta}

import scala.collection.mutable.ListBuffer

case class Match(matchee: Expression, cases: Seq[Case]) extends Statement {
  override def boundVars: Set[Name] = cases.flatMap(_.boundVars).toSet
  override def allVars: Map[Name, Option[Type]] = matchee.freeVars ++ cases.flatMap(_.allVars)

  override def prettyprint(implicit indent: String): String = {
    val casesS = if (cases.isEmpty) "" else
    "\n" + cases.map(_.prettyprint(indent+Meta.TAB)).mkString("\n")
    s"""${indent}${matchee.prettyprint} match {$casesS
       |${indent}}""".stripMargin
  }

}
case class Case(pattern: Pattern, body: Body) extends SourceLocation {
  def boundVars: Set[Name] = pattern.boundVars ++ body.boundVars
  def allVars: Map[Name, Option[Type]] = pattern.allVars ++ body.allVars

  def prettyprint(implicit indent: String): String =
    s"${indent}case ${pattern.prettyprint} => ${body.prettyprint}"
}

sealed trait Pattern extends SourceLocation {
  def boundVars: Set[Name]
  def allVars: Map[Name, Option[Type]]
  def prettyprint(implicit indent: String): String
}

case class NodePattern(c: TNode, bindings: Seq[PatternBinding]) extends Pattern {
  def boundVars: Set[Name] = bindings.flatMap(_.pattern.boundVars).toSet
  override def allVars: Map[Name, Option[Type]] = bindings.flatMap(_.pattern.allVars).toMap

  override def prettyprint(implicit indent: String): String = {
    val bindingsS = if (bindings.isEmpty) "" else
      bindings.map(_.prettyprint).mkString(", ")
    s"${c.prettyprint}($bindingsS)"
  }
}
case class PatternBinding(field: Name, pattern: Pattern) extends Typeable with SourceLocation {
  def prettyprint(implicit indent: String): String =
    s"$field = ${pattern.prettyprint}"
}

case class TuplePattern(pats: Seq[Pattern]) extends Pattern {
  override def boundVars: Set[Name] = pats.flatMap(_.boundVars).toSet
  override def allVars: Map[Name, Option[Type]] = pats.flatMap(_.allVars).toMap
  override def prettyprint(implicit indent: String): String =
    if (pats.isEmpty)
      "()"
    else if (pats.size == 1)
      pats.head.prettyprint
    else
      pats.map(_.prettyprint).mkString("(", ", ", ")")
}

case class VarPattern(name: Name) extends Pattern with Var.Target {
  override def boundVars: Set[Name] = Set(name)
  override def allVars: Map[Name, Option[Type]] = Map(name -> None)
  override def prettyprint(implicit indent: String): String = name.name
}
case class NamedPattern(name: Name, pat: Pattern) extends Pattern with Var.Target {
  override def boundVars: Set[Name] = Set(name) ++ pat.boundVars
  override def allVars: Map[Name, Option[Type]] = Map(name -> None) ++ pat.allVars
  override def prettyprint(implicit indent: String): String = s"$name@${pat.prettyprint}"
}

case object WildcardPattern extends Pattern {
  override def boundVars: Set[Name] = Set()
  override def allVars: Map[Name, Option[Type]] = Map()
  override def prettyprint(implicit indent: String): String = "_"
}

case class LiteralPattern(v: Literal) extends Pattern {
  override def boundVars: Set[Name] = Set()
  override def allVars: Map[Name, Option[Type]] = Map()
  override def prettyprint(implicit indent: String): String = v.prettyprint
}


/**
 * Extension adding pattern matching statements to @see CoreParser.
 */
trait MatchFrontend extends Frontend {
  import fastparse.ScalaWhitespace._
  import fastparse._

  override protected def desugarables: Seq[Desugarable] = Match +: super.desugarables

  override protected[frontend] def keywords: Set[String] = super.keywords ++ Seq("match", "case")

  /**
   * Statement parser
   */
  override protected[frontend] def statement[_: P]: P[Statement] =
    matchStatement | super.statement

  protected[frontend] def matchStatement[_: P]: P[Statement] =
    P(exp ~ "match" ~ "{" ~ P(sp ~~ case_ ~~ sp).repX(sep = nl_!) ~ "}").mapWithLoc { case (e, cs) => Match(e, cs) }

  protected[frontend] def case_[_: P]: P[Case] =
    P("case " ~ pattern ~ "=>" ~ body).mapWithLoc(Case.tupled)

  protected[frontend] def pattern[_: P]: P[Pattern] =
    P(tuplePattern | namedPattern | nodePattern | wildcardPattern | varPattern
      | literalPattern)

  protected[frontend] def patternBinding[_: P]: P[PatternBinding] =
    P(identifier ~ "=" ~ pattern).mapWithLoc(PatternBinding.tupled) |
    P(identifier).mapWithLoc(name => PatternBinding(name, VarPattern(name)))

  protected[frontend] def nodePattern[_: P]: P[Pattern] =
    P(tNode ~ "(" ~ P(patternBinding).rep(sep = ",") ~ ")").mapWithLoc(NodePattern.tupled)

  protected[frontend] def tuplePattern[_: P]: P[Pattern] =
    P("(" ~ P(pattern).rep(sep = ",") ~ ")").mapWithLoc(TuplePattern)

  protected[frontend] def varPattern[_: P]: P[Pattern] =
    P(identifier).mapWithLoc(VarPattern)

  protected[frontend] def namedPattern[_: P]: P[Pattern] =
    P(identifier ~ "@" ~ pattern).mapWithLoc(NamedPattern.tupled)

  protected[frontend] def wildcardPattern[_: P]: P[Pattern] =
    P("_").!.mapWithLoc(_ => WildcardPattern)

  protected[frontend] def literalPattern[_: P]: P[Pattern] =
    P(literal).mapWithLoc(LiteralPattern)

  override protected def typecheckInternal(stm: Statement, mustYield: Boolean): StmType = stm match {
    case Match(matchee, cases) =>
      val mty = typecheck(matchee)
      val ctys = cases.map { c =>
        scopedTypeContext {
          typecheckPattern(c.pattern, mty)
          typecheck(c.body, mustYield)
        }
      }
      if (cases.isEmpty)
        NoYield
      else
        ctys.reduce(stmMeet(_, _, lang))

    case _ => super.typecheckInternal(stm, mustYield)
  }

  def typecheckPattern(pattern: Pattern, matchee: Type): Unit = pattern match {
    case NodePattern(node, bindings) =>
      if (meet(node, matchee, lang) == TNothing)
        warn(s"Type of pattern $node unrelated type to matchee type $matchee", pattern)

      bindings.foreach { case b@PatternBinding(field, pattern) =>
        assignType(b) {
          lang.links.get(node.name, field.name) match {
            case Some(trueType) =>
              val ty = truechangeTypeToType(trueType)
              typecheckPattern(pattern, ty)
              ty
            case None => lang.litLinks.get(node.name, field.name) match {
              case Some(trueLitType) =>
                val ty = TLiteral(trueLitType)
                typecheckPattern(pattern, ty)
                ty
              case None =>
                error(s"Cannot access field `$field` of node $node", field)
                typecheckPattern(pattern, TAny)
                TAny
            }
          }
        }
      }

    case TuplePattern(pats) =>
      matchee match {
        case TUnit =>
          if (pats.nonEmpty)
            warn(s"Cannot match expression of type $TUnit against ${pats.size}-ary tuple pattern", pattern)
        case TTuple(tys) =>
          if (pats.size != tys.size)
            warn(s"Cannot match ${tys.size}-ary tuple against ${pats.size}-ary tuple pattern", pattern)
          pats.zipAll(tys, null, null).foreach {
            case (pat, null) => typecheckPattern(pat, TAny)
            case (null, ty) => // nothing
            case (pat, ty) => typecheckPattern(pat, ty)
          }
        case ty =>
          if (pats.size != 1)
            warn(s"Cannot match expression of type $ty against ${pats.size}-ary tuple pattern", pattern)
          pats.zipAll(Seq(ty), null, null).foreach {
            case (pat, null) => typecheckPattern(pat, TAny)
            case (null, ty) => // nothing
            case (pat, ty) => typecheckPattern(pat, ty)
          }
      }

    case vp@VarPattern(name) =>
      bindVar(name, vp, matchee)
    case np@NamedPattern(name, pat) =>
      bindVar(name, np, matchee)
      typecheckPattern(pat, matchee)
    case WildcardPattern =>
      // nothing
    case LiteralPattern(v) =>
      val ty = typecheckLiteral(v)
      if (meet(ty, matchee, lang) == TNothing)
        warn(s"Type of pattern $ty unrelated type to matchee type $matchee", pattern)
  }
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

    def desugarPat(exp: Expression, pat: Pattern)(implicit gensym: Gensym): Seq[Statement] = pat match {
      case NodePattern(c, bindings) =>
        val result = ListBuffer[Statement]()
        val matchee: Var = {
          val sym = Name(gensym.fresh("matchee"))
          result += Assign(Seq(sym), Cast(exp, c))
          Var(sym).typed(c)
        }
        bindings.foreach { case binding@PatternBinding(field, subpat) =>
          val typ = binding.typ.getOrElse(throw new IllegalArgumentException(s"Cannot desugar untyped pattern binding $binding"))
          result ++= desugarPat(PathAccess(matchee, NamedLink(field)).typed(typ), subpat)
        }
        result.toSeq
      case TuplePattern(pats) =>
        val result = ListBuffer[Statement]()
        val syms = pats.indices.map(i => Name(gensym.fresh(s"matchee_tuple$i")))
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

    def desugarNegatedPat(exp: Expression, pat: Pattern)(implicit gensym: Gensym): Seq[Seq[Statement]] = pat match {
      case NodePattern(c, bindings) =>
        val (ensureVar, ensureVarCasted, matchee, matcheeCasted) = exp match {
          case v: Var =>
            val sym = Name(gensym.fresh("matchee"))
            (Seq(), Seq(Assign(Seq(sym), Cast(exp, c))), v, Var(sym))
          case _ =>
            val sym = Name(gensym.fresh("matchee"))
            (Seq(Assign(Seq(sym), exp)), Seq(Assign(Seq(sym), Cast(exp, c))), Var(sym), Var(sym))
        }

        val wrongType = ensureVar :+ Assert(NotInstanceOf(matchee, c))
        val alts = bindings.flatMap { case binding@PatternBinding(field, subpat) =>
          val typ = binding.typ.getOrElse(throw new IllegalArgumentException(s"Cannot desugar untyped pattern binding $binding"))
          val patAlts = desugarNegatedPat(PathAccess(matcheeCasted, NamedLink(field)).typed(typ), subpat)
          patAlts.map(ensureVarCasted ++ _)
        }
        wrongType +: alts
      case TuplePattern(pats) =>
        val syms = pats.indices.map(i => Name(gensym.fresh(s"matchee_tuple$i")))
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