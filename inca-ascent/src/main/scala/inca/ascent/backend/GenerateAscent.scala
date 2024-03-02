package inca.ascent.backend

import inca.ascent.backend.GenerateAscent.cleanName
import inca.ascent.syntax.Term.{NumberLit, Var}
import inca.ascent.syntax.Unop.neg
import inca.ascent.syntax.Aggregation.Count
import inca.ascent.syntax.BinOp.Add
import inca.ascent.syntax.Condition.Equal
import inca.ascent.syntax.*
import inca.ir
import inca.ir.extension.aggregate.{AggregateColumnArg, AggregationOperator}
import inca.ir.extension.{string, aggregate as agg, arithmetic as arith}
import inca.ir.{Arg, TermArg, WildcardArg}
import inca.ir.extension.data
import inca.ir.extension.data.*
import inca.ir.{Name, name2string, string2name}
import inca.util.Gensym
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator.{MaxDouble, MaxInt, MinDouble, MinInt, SumDouble, SumInt, Count as CountAgg}
import inca.ir.typing.Mode.{Binding, Bound, Collapse}

// Based on Sarah Hauschildts

object GenerateAscent:
  def cleanName(name: ir.Name): String = name.name.replace("$", "_")

class GenerateAscent:
  private val gensym = new Gensym()

  private var callVars: Map[String, Seq[Term.Var]] = Map()

  def compileModule(module: ir.Module): Seq[ProgramContent] = {

    val compileableFeatures = Set(ir.BaseIR, arith.IR, string.IR, data.IR, agg.IR)
    val illegalFeatures = module.lang.features -- compileableFeatures

    if (illegalFeatures.nonEmpty)
      throw IllegalArgumentException(s"[ERROR]: Cannot compile module containing the following features: ${illegalFeatures.mkString(", ")}")

    // TData -> CaseDef
    val caseDefs = module.contents.collect {
      case cd: CaseDefinition => cd.data.ref.name -> cd
    }.groupBy(_._1).view.mapValues(e => e.map(x => x._2)).toMap

    val contents = module.contents.flatMap {
      case ir.Relation(name, param, bodies) =>
        val param_type = param.map { p => compileType(p.ty) }
        val param_var = param.map { p => (Term.Var(cleanName(p.name)), compileType(p.ty)) }
        val relDecl = ProgramContent.RelDecl(cleanName(name), param_type)
        val rules = relDecl +: bodies.zipWithIndex.map((p, idx) =>
          val r = cleanName(name)
          val rname = s"$r$idx"
          callVars = callVars + (rname -> Seq())
          ProgramContent.Rule(cleanName(name), param_var, compileBody(p)(rname)))
        rules

      case ir.ExtensionalRelation(name, params) =>
        val param_type = params.map { p => compileType(p.ty) }
        Seq(ProgramContent.RelDecl(cleanName(name), param_type))

      case data.DataDefinition(name) =>
        val compiledCases = caseDefs(name).map {
          case CaseDefinition(caseName, caseTypes, _) =>
            cleanName(caseName.name) -> caseTypes.map(compileType)
        }
        Seq(ProgramContent.CustomType(cleanName(name), compiledCases))

      case _ => Seq()
    }

    contents
  }


  private def compileBody(body: ir.Body)(implicit rname: String): Seq[Atom] = body.atoms.flatMap(p => compileAtom(p))

  private def compileAggOp(op: AggregationOperator, aggregatorVar: ir.Var)(implicit rname: String): Aggregation = op match
    case MinInt | MinDouble => Aggregation.Min(compileTerm(aggregatorVar))
    case MaxInt | MaxDouble => Aggregation.Max(compileTerm(aggregatorVar))
    case SumInt | SumDouble => Aggregation.Sum(compileTerm(aggregatorVar))
    case CountAgg => Aggregation.Count()
    case _ => throw new IllegalArgumentException(s"Unknown aggregation operation: $op")

  private def compileAtom(atom: ir.Atom)(implicit rname: String): Seq[Atom] = atom match {
    case ir.Eq(lhs, rhs, false) =>
      (lhs, rhs) match {
        case (string.StringConcat(_, _), string.StringConcat(_, _)) =>
          Seq(Atom.ConditionalClause(Condition.Equal(compileTerm(lhs), compileTerm(rhs))))
        case (string.StringConcat(_, _), _) =>
          Seq(Atom.Let((compileTerm(rhs), FormatType.Symbol), compileTerm(lhs)))
        case (_, string.StringConcat(_, _)) =>
          Seq(Atom.Let((compileTerm(lhs), FormatType.Symbol), compileTerm(rhs)))
        case (data.Construct(_, _), data.Construct(_, _)) =>
          Seq(Atom.ConditionalClause(Condition.Equal(compileTerm(lhs), compileTerm(rhs))))
        case (data.Construct(ref, _), _) =>
          val caseDef = ref.target.get
          val dataDef = caseDef.data.ref.target.get
          val dataName = cleanName(dataDef.name)
          Seq(Atom.Let((compileTerm(rhs), FormatType.Custom(dataName)), compileTerm(lhs)))
        case (_, data.Construct(ref, _)) =>
          val caseDef = ref.target.get
          val dataDef = caseDef.data.ref.target.get
          val dataName = cleanName(dataDef.name)
          Seq(Atom.Let((compileTerm(lhs), FormatType.Custom(dataName)), compileTerm(rhs)))
        case (ir.Var(_), arith.BinOp(_, _, _)) =>
          Seq(Atom.Let((compileTerm(lhs), FormatType.Number), compileTerm(rhs)))
        case (arith.BinOp(_, _, _), ir.Var(_)) =>
          Seq(Atom.Let((compileTerm(rhs), FormatType.Number), compileTerm(lhs)))
        case _ => (lhs.typ, rhs.typ) match {
          case (Some(ir.TermType(ty1, m1)), Some(ir.TermType(ty2, m2))) =>
            (m1, m2) match {
              case (Bound, Bound) =>
                Seq(Atom.ConditionalClause(Condition.Equal(compileTerm(lhs), compileTerm(rhs))))
              case (Bound, Binding) =>
                Seq(Atom.Let((compileTerm(rhs), compileType(ty2)), compileTerm(lhs)))
              case (Binding, Bound) =>
                Seq(Atom.Let((compileTerm(lhs), compileType(ty1)), compileTerm(rhs)))
              case _ =>
                throw new RuntimeException(s"Unexpected binding for terms: $lhs and $rhs")
            }
          case _ => throw new RuntimeException("Untyped terms !")
        }
      }
    case ir.Eq(lhs, rhs, true) =>
      Seq(Atom.ConditionalClause(Condition.NotEqual(compileTerm(rhs), compileTerm(lhs))))
    case arith.BinCompare(lhs, rhs, "<") =>
      Seq(Atom.ConditionalClause(Condition.LesserThan(compileTerm(lhs), compileTerm(rhs))))
    case arith.BinCompare(lhs, rhs, ">") =>
      Seq(Atom.ConditionalClause(Condition.GreaterThan(compileTerm(lhs), compileTerm(rhs))))
    case arith.BinCompare(lhs, rhs, "<=") =>
      Seq(Atom.ConditionalClause(Condition.LesserThanEqual(compileTerm(lhs), compileTerm(rhs))))
    case arith.BinCompare(lhs, rhs, ">=") =>
      Seq(Atom.ConditionalClause(Condition.GreaterTHanEqual(compileTerm(lhs), compileTerm(rhs))))
    case ir.Call(name, args, false) =>
      val argParam = args.map(a => compileArg(a))
      val param = argParam.flatMap {
        case p: Term.Var => Seq(p)
        case _ => None
      }
      callVars += rname -> (callVars.getOrElse(rname, Seq()) ++ param)
      Seq(Atom.Call(cleanName(name.name), argParam))

    case ir.ExtensionalCall(name, args, false) =>
      val argParam = args.map(a => compileArg(a))
      val param = argParam.flatMap {
        case p: Term.Var => Seq(p)
        case _ => None
      }
      callVars += rname -> (callVars.getOrElse(rname, Seq()) ++ param)
      Seq(Atom.Call(cleanName(name.name), argParam))
    case ir.Call(name, args, true) =>
      val arg_param = args.map(a => compileArg(a))
      val param = arg_param.flatMap {
        case p: Term.Var => Seq(p)
        case _ => None
      }
      callVars += rname -> (callVars.getOrElse(rname, Seq()) ++ param)
      Seq(Atom.Not(Atom.Call(cleanName(name.name), arg_param)))
    case ir.ExtensionalCall(name, args, true) =>
      val arg_param = args.map(a => compileArg(a))
      val param = arg_param.flatMap {
        case p: Term.Var => Seq(p)
        case _ => None
      }
      callVars += rname -> (callVars.getOrElse(rname, Seq()) ++ param)
      Seq(Atom.Not(Atom.Call(cleanName(name.name), arg_param)))

    case data.Deconstruct(t, name, args, neg) =>
      val tmp = cleanName(gensym.fresh("tmp"))
      val arg = args.map(p => compileArg(p))
      Seq(Atom.Deconstruct(compileTerm(t), cleanName(name.name), tmp, arg, neg))

    case agg.Aggregate(name, args, op) =>
      val result = args.zipWithIndex.collect {
        case (col: agg.AggregateColumnArg, idx) => col -> idx
      }
      val (aggregationColumn, resultIdx) = result.head

      val aggregatorVar = ir.Var(cleanName(gensym.fresh("x")))
      val replacedArgs = args.patch(resultIdx, Seq(TermArg(aggregatorVar)), 1)
      val replacedArgsforCount = args.patch(resultIdx, Seq(WildcardArg()), 1)
      val ascentAgg = compileAggOp(op, aggregatorVar)
      val callArgs = ascentAgg match
        case Aggregation.Count() => replacedArgsforCount.map(t => compileArg(t))
        case _ => replacedArgs.map(t => compileArg(t))
      val refname = name.name
      val aggrcontent = aggregationColumn match {
        case agg.AggregateColumnArg(res@ir.Var(resultVar)) =>
          res.typ match
            case Some(ir.TermType(ty, m)) => m match
              case Bound =>
                val tmpName = cleanName(gensym.fresh("tmp"))
                Seq(
                  Atom.Aggregator(cleanName(tmpName), ascentAgg, Atom.Call(cleanName(refname), callArgs)),
                  Atom.ConditionalClause(Condition.Equal(Term.Var(tmpName), Term.Var(cleanName(resultVar.name))))
                )
              case Binding =>
                Seq(
                  Atom.Aggregator(cleanName(resultVar.name), ascentAgg, Atom.Call(cleanName(refname), callArgs))
                )
              case Collapse =>
                throw new RuntimeException("Unexpected collapsed term as aggregation output!")
            case ty =>
              throw new RuntimeException("Untyped aggregation output!")
        case agg.AggregateColumnArg(t) =>
          val tmpName = cleanName(gensym.fresh("tmp"))
          Seq(
            Atom.Aggregator(cleanName(tmpName), ascentAgg, Atom.Call(cleanName(refname), callArgs)),
            Atom.ConditionalClause(Condition.Equal(Term.Var(tmpName), compileTerm(t), t_agg = true))
          )
        case _ => throw new IllegalArgumentException(s"No Aggregation column")
      }
      aggrcontent
  }

  private def compileArg(arg: ir.Arg)(implicit rname: String): Term = arg match
    case AggregateColumnArg(t) => compileTerm(t)
    case TermArg(t) => compileTerm(t)
    case WildcardArg() => Term.Wildcard
    case _ => throw new RuntimeException(s"Unsupported arg: $arg")

  private def compileBinOp(op: String): BinOp = op match
    case "+" => BinOp.Add
    case "-" => BinOp.Sub
    case "*" => BinOp.Mul
    case "/" => BinOp.Div
    case "%" => BinOp.Rem
    case _ => throw new RuntimeException("Unsupported binary operation: " + op)

  private def compileTerm(t: ir.Term)(implicit rname: String): Term = t match {
    case ir.Var(name) => Term.Var(cleanName(name.name))
    case ir.Cast(t, ty) => Term.TypeCast(compileTerm(t), compileType(ty))
    case arith.IntNum(n) => Term.NumberLit(n)
    case arith.DoubleNum(n) => Term.FloatLit(n.toFloat)
    case arith.BinOp(lhs, rhs, op) => Term.Binary(compileTerm(lhs), compileBinOp(op), compileTerm(rhs), callVars(rname))
    case string.StringLit(s) => Term.StringLit(s)
    case string.ToString(t) => Term.ToString(compileTerm(t))
    case string.StringConcat(t1, t2) => Term.Concat(Seq(compileTerm(t1), compileTerm(t2)))
    case arith.UnOp(t, "-") => Term.Unary(Unop.neg, compileTerm(t))
    case data.Construct(ref, args) =>
      val caseDef = ref.target.get
      val dataDef = caseDef.data.ref.target.get
      val dataName = cleanName(dataDef.name)
      val pty = caseDef.args.map(compileType)
      Term.CustomLit(dataName, cleanName(caseDef.name), pty, args.map(p => compileTerm(p)), callVars(rname))
  }

  private def compileType(ty: ir.Type): FormatType = ty match
    case arith.TInt => FormatType.Number
    case arith.TDouble => FormatType.Float
    case string.TString => FormatType.Symbol
    case data.TData(name) => FormatType.Custom(cleanName(name.name))
