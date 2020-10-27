package inca.frontend.typechecker.extensions

import inca.frontend.core.Core
import inca.frontend.core.Core._
import inca.frontend.extensions._
import inca.frontend.typechecker.{CoreTypechecker, TypeContext, TypeError, TypecheckerExtension}
import inca.frontend.util.TypeHelper

/** Match Typechecker Extension
  *
  * @author  Ronja Schnur (rschnur@students.uni-mainz.de)
  *          Julian Cichorius (jcichori@students.uni-mainz.de)
  */
object MatchTypechecker extends TypecheckerExtension {

  def typecheck(c: TNode, pb: PatternBinding)(implicit
      context: TypeContext
  ): CoreTypechecker.TypeEnvironment = {
    val PatternBinding(field, pattern) = pb

    typechecker.lmi.links.get((c.prettyprint, field.name)) match {
      case None =>
        typechecker.addError(TypeError.undefined(s"${c.prettyprint}.$field", field, "Match"))
        typecheck(pattern)._2
      case Some(typ) =>
        val patternType = TypeHelper.decode(typ.toString)
        pb.typed(patternType)
        typecheck(pattern, patternType)._2
    }
  }

  private def typecheck(pat: Pattern, estm: TypeAnno = TAny)(implicit
      context: TypeContext
  ): (TypeAnno, CoreTypechecker.TypeEnvironment) = {
    pat match {
      case NodePattern(c, bindings) =>
        val envs = bindings.map(typecheck(c, _))
        (c, envs.foldLeft(context.tenv)({case (a, b) => typechecker.union(a, b)}))

      case TuplePattern(pats) =>
        val (tp, tenvs) = pats.map(typecheck(_)).unzip
        (
          TTuple(tp),
          tenvs.foldLeft(context.tenv)({ case (a, b) => typechecker.union(a, b) })
        )
      case VarPattern(name) =>
        if (context.tenv.contains(name)) {
          typechecker.addError(TypeError.alreadyUsed(name, "Match"))
          (estm, context.tenv + (name -> estm))
        } else
          (estm, context.tenv + (name -> estm))
      case NamedPattern(name, pat) =>
        if (context.tenv.contains(name)) {
          typechecker.addError(TypeError.alreadyUsed(name, "Match"))
          (estm, context.tenv)
        } else {
          val (t, ne) = typecheck(pat, estm)
          (t, typechecker.union(context.tenv + (name -> t), ne))
        }
      case LiteralPattern(v) =>
        val t = typechecker.typecheck(v)
        if (!typechecker.subtype(t, estm))
          typechecker.addError(TypeError.expected(estm, t, "Match"))
        (t, context.tenv)
      case WildcardPattern => (estm, context.tenv)
      case _ =>
        typechecker.addError(TypeError.undefined("Pattern", Name(s"${pat.prettyprint("")}"), "Match"))
        (estm, context.tenv)
    }
  }

  private def typecheck(matchee_t: TypeAnno, cs: Case)(implicit context: TypeContext): TypeAnno = {
    val lmi = typechecker.lmi
    val Case(pattern, body) = cs
    val (t, te) = typecheck(pattern, matchee_t)

    if (!typechecker.subtype(t, matchee_t))
      typechecker.addError(TypeError.expected(matchee_t, t, "Match"))
    
    typechecker.typecheck(body)(new TypeContext(context, te))
  }

  override def typecheck(s: Core.Statement, last_in_body: Boolean)(implicit context: TypeContext):
  (Option[Core.TypeAnno], CoreTypechecker.TypeEnvironment, Boolean) = {
    s match {
      case Match(matchee, cases) =>
        val (t, te) = typechecker.typecheck(matchee)
        val return_types = cases.map(typecheck(t, _))

        if (last_in_body) {
          // check if all return values are the same
          val resType = typechecker.meet(return_types) match {
            case None =>
              typechecker.addError(TypeError.incompatibleReturnTypes("Match"))
              TUnit
            case Some(t) =>
              t
          }

          (Some(resType), te, true)
        } else
          (None, te, true)
      case _ => (None, context.tenv, false)
    }
  }
}
