package inca.frontend.constraint.extensions.match_

import inca.frontend.constraint.core._
import inca.frontend.constraint.desugar.{DesugarTrans, Desugarable}
import inca.frontend.constraint.extensions.match_.Trees._
import inca.frontend.constraint.extensions.switch_
import inca.util.Gensym
import inca.util.Meta.Scala

import scala.collection.mutable.ListBuffer

object Desugaring extends Desugarable {

  override val desugarsTo: Seq[Desugarable] = Seq(switch_.Desugaring)
  import switch_.Trees._

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

      case ScalaPattern(Eval(Scala(fun)), noArgs, args) =>
        val result = ListBuffer[Statement]()
        val matchee: Name = exp match {
          case v: Var => v.name
          case _ =>
            val sym = Name(gensym.fresh("matchee"))
            result += Assign(Seq(sym), exp)
            Var(sym).name
        }
        val matcheeTerm = meta.Term.Name(matchee.name)

        import meta.quasiquotes._
        if (noArgs) {
          result += Assert(Eval(Seq(EvalParam(matchee)), Scala(q"$matcheeTerm == $fun")).typed(TScalaBoolean))
        } else {
          val vars = args.map(_ => meta.Term.Name(gensym.fresh("scalaPatArg"))).toList
          val matchCode = meta.Term.Match(matcheeTerm, List(
            meta.Case(meta.Pat.Extract(fun, vars.map(_ => meta.Pat.Wildcard())), None, q"true"),
            meta.Case(meta.Pat.Wildcard(), None, q"false")
          ))
          result += Assert(Eval(Seq(EvalParam(matchee)), Scala(matchCode)).typed(TScalaBoolean))
          args.zip(vars).foreach { case (arg, v) =>
            val code = meta.Term.Match(matcheeTerm, List(
              meta.Case(meta.Pat.Extract(fun, vars.map(meta.Pat.Var.apply)), None, v),
              meta.Case(meta.Pat.Wildcard(), None, meta.Lit.Null())))
            val subMatchee = Eval(Seq(EvalParam(matchee)), Scala(code)).mtyped(pat.typ)
            result ++= desugarPat(subMatchee, arg)
          }
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

      case ScalaPattern(Eval(Scala(fun)), noArgs, args) =>
        val (ensureVar, matchee) = exp match {
          case v: Var => (Seq(), v.name)
          case _ =>
            val sym = Name(gensym.fresh("matchee"))
            (Seq(Assign(Seq(sym), exp)), Var(sym).name)
        }
        val matcheeTerm = meta.Term.Name(matchee.name)

        import meta.quasiquotes._
        if (noArgs) {
          val mismatch = ensureVar :+ Assert(Eval(Seq(EvalParam(matchee)), Scala(q"$matcheeTerm != $fun")).typed(TScalaBoolean))
          Seq(mismatch)
        } else {
          val vars = args.map(_ => meta.Term.Name(gensym.fresh("scalaPatArg"))).toList
          val mistmatchCode = meta.Term.Match(matcheeTerm, List(
            meta.Case(meta.Pat.Extract(fun, vars.map(_ => meta.Pat.Wildcard())), None, q"false"),
            meta.Case(meta.Pat.Wildcard(), None, q"true")
          ))
          val mismatch = ensureVar :+ Assert(Eval(Seq(EvalParam(matchee)), Scala(mistmatchCode)).typed(TScalaBoolean))
          val alts = args.zip(vars).flatMap { case (arg, v) =>
            val code = meta.Term.Match(matcheeTerm, List(
              meta.Case(meta.Pat.Extract(fun, vars.map(meta.Pat.Var.apply)), None, v),
              meta.Case(meta.Pat.Wildcard(), None, meta.Lit.Null())
            ))
            val subMatchee = Eval(Seq(EvalParam(matchee)), Scala(code)).mtyped(pat.typ)
            desugarNegatedPat(subMatchee, arg).map(ensureVar ++ _)
          }
          mismatch +: alts
        }

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