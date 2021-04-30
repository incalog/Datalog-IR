package inca.frontend.extensions.match_

import inca.frontend.core.tree._
import inca.frontend.extensions.match_.Trees._
import inca.frontend.typechecker.{CoreTypechecker, NoYield, StmType}
import inca.frontend.util.TypeHelper

trait Typechecker extends CoreTypechecker {
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
        ctys.reduce(stmMeet(_, _, dataModel))

    case _ => super.typecheckInternal(stm, mustYield)
  }

  def typecheckPattern(pattern: Pattern, matchee: Type): Unit = pattern match {
    case NodePattern(node, bindings) =>
      validType(node)
      if (meet(node, matchee, dataModel) == TNothing)
        warn(s"Type of pattern $node unrelated type to matchee type $matchee", pattern)

      bindings.foreach { case b@PatternBinding(field, pattern) =>
        assignType(b) {
          dataModel.links.get(node.name -> field.name) match {
            case Some(trueType) =>
              val ty = truechangeTypeToType(trueType)
              typecheckPattern(pattern, ty)
              ty
            case None => dataModel.litLinks.get(node.name -> field.name) match {
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

    case ScalaPattern(fun, noArgs, args) =>
      if (!fun.code.tree.isExtractor)
        error(s"Expected Scala extractor", fun)

      def decode(t: meta.Type): Type =
        TypeHelper.decode(t) match {
          case Left(err) => error(err, pattern); TAny
          case Right(ty) => ty
        }

      if (noArgs) {
        val tyString = typecheckScala(fun.code.syntax) match {
          case Left(ty) => ty
          case Right(err) =>
            error(err.getMessage, pattern)
            "Any"
        }

        import scala.meta.parsers._
        val expected = decode(tyString.parse[meta.Type].get)
        if (meet(expected, matchee, dataModel) == TNothing)
          warn(s"Type of pattern $fun unrelated type to matchee type $matchee", pattern)
      } else {
        val funTyString = typecheckScala(s"${fun.code.tree}.unapply _") match {
          case Left(ty) => ty
          case Right(err) =>
            error(err.getMessage, pattern)
            "Any"
        }

        import scala.meta.parsers._
        val (expected, params) = funTyString.parse[meta.Type].get match {
          case meta.Type.Function(Seq(expected), meta.Type.Apply(_, Seq(result))) =>
            result match {
              case meta.Type.Tuple(ts) => (decode(expected), ts.map(decode))
              case _ => (decode(expected), Seq(decode(result)))
            }
          case ty =>
            error(s"Unexpected unapply signature $funTyString for pattern $fun", pattern)
            (matchee, Seq(decode(ty)))
        }

        if (meet(expected, matchee, dataModel) == TNothing)
          warn(s"Type of pattern $fun unrelated type to matchee type $matchee", pattern)
        if (params.size != args.size)
          error(s"Function $fun expects ${params.size} arguments, but found ${args.size} arguments in pattern", pattern)

        params.zipAll(args, null, null) foreach {
          case (null, arg) =>
          // typecheck(arg)
          case (param, null) =>
          // nothing
          case (param, arg) =>
            typecheckPattern(arg, param)
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
      if (meet(ty, matchee, dataModel) == TNothing)
        warn(s"Type of pattern $ty unrelated type to matchee type $matchee", pattern)
  }
}
