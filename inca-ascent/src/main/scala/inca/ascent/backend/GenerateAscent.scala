package inca.ascent.backend

import inca.ascent.backend.GenerateAscent.cleanName
import inca.ascent.syntax.Term.{NumberLit, Var}
import inca.ascent.syntax.Unop.neg
import inca.ascent.syntax.Aggregation.Count
import inca.ascent.syntax.BinOp.Add
import inca.ascent.syntax.*
import inca.ir
import inca.ir.extension.aggregate.{AggregateColumnArg, AggregationOperator}
import inca.ir.extension.{string, aggregate as agg, arithmetic as arith}
import inca.ir.{Arg, Name, TAny, TermArg, WildcardArg, name2string, string2name}
import inca.ir.extension.data
import inca.ir.extension.data.*
import inca.ir.extension.string.TString
import inca.util.Gensym
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator.{MaxDouble, MaxInt, MinDouble, MinInt, SumDouble, SumInt, Count as CountAgg}
import inca.ir.typing.Mode.{Binding, Bound, Collapse}

import scala.annotation.tailrec

// Based on Sarah Hauschildts Bachelor thesis

object GenerateAscent:
  private val gensym = new Gensym()
  private var varRefs: Set[String] = Set()

  def cleanName(name: ir.Name): String = name.name.replace("$", "_")

  def scoped[A](f: => A): A = gensym.scoped {
    val oldRefs = this.varRefs
    try {
      val a = f
      a
    } finally {
      this.varRefs = oldRefs
    }
  }

  private def freshTmpName(): String = cleanName(gensym.fresh("_tmp"))

  private var typeDependencies: Map[String, Set[String]] = Map()

  private def addTransitive[A, B](s: Set[(A, B)]) =
    s ++ (for ((x1, y1) <- s; (x2, y2) <- s if y1 == x2) yield (x1, y2))

  @tailrec
  private def transitiveClosure[A, B](s: Set[(A, B)]): Set[(A, B)] = {
    val t = addTransitive(s)
    if (t.size == s.size) s else transitiveClosure(t)
  }

  def compileModule(module: ir.Module): Seq[ProgramContent] = gensym.scoped {
    val compileableFeatures = Set(ir.BaseIR, arith.IR, string.IR, data.IR, agg.IR)
    val illegalFeatures = module.lang.features -- compileableFeatures

    if (illegalFeatures.nonEmpty)
      throw IllegalArgumentException(s"[ERROR]: Cannot compile module containing the following features: ${illegalFeatures.mkString(", ")}")

    // TData -> CaseDef
    val caseDefs = module.contents.collect {
      case cd: CaseDefinition => cd.data.ref.name -> cd
    }.groupBy(_._1).view.mapValues(e => e.map(x => x._2)).toMap

    // Find indirect recursive data types, since we need to box them
    val nestedDataTypes = caseDefs.toSet.flatMap { (dataName, cases) =>
      cases.flatMap(_.args.collect {
        case TData(ref) => cleanName(dataName) -> cleanName(ref.name.name)
      })
    }

    typeDependencies = transitiveClosure(nestedDataTypes).groupBy(_._1).map { case (k,v) => (k,v.map(_._2))}

    val contents = module.contents.flatMap {
      case ir.Relation(name, param, bodies) =>
        val pTy = param.map { p => compileType(p.ty) }
        val ps = param.map {
          case ir.Param(name, ty@TData(_)) =>
            (Term.Clone(Term.Var(cleanName(name))), compileType(ty))
          case ir.Param(name, ty@TString) =>
            (Term.Clone(Term.Var(cleanName(name))), compileType(ty))
          case ir.Param(name, ty) =>
            (Term.Var(cleanName(name)), compileType(ty))
        }
        val relDecl = ProgramContent.RelDecl(cleanName(name), pTy)
        val rules = relDecl +: bodies.zipWithIndex.map((p, idx) => scoped {
          val r = cleanName(name)
          ProgramContent.Rule(cleanName(name), ps, compileBody(p))
        })
        rules

      case ir.ExtensionalRelation(name, params) =>
        val param_type = params.map { p => compileType(p.ty) }
        Seq(ProgramContent.RelDecl(cleanName(name), param_type, true))

      case data.DataDefinition(dataName) =>
        val cleanDataName = cleanName(dataName)
        val compiledCases = caseDefs(dataName).map {
          case CaseDefinition(caseName, caseTypes, _) =>
            val cleanCaseName = cleanName(caseName.name)
            cleanCaseName -> caseTypes.map { ty =>
              compileType(ty, Some(cleanDataName))
            }
        }
        Seq(ProgramContent.CustomType(cleanDataName, compiledCases))

      case _ => Seq()
    }

    contents
  }

  private def compileBody(body: ir.Body): Seq[Atom] = gensym.scoped {
    val allVars = body.atoms.flatMap(_.vars.map(_.name.name))
    gensym.register(allVars)
    body.atoms.flatMap(p => compileAtom(p))
  }

  private def compileAggOp(op: AggregationOperator, aggregatorVar: ir.Var): Aggregation = op match
    case MinInt | MinDouble => Aggregation.Min(compileTerm(aggregatorVar))
    case MaxInt | MaxDouble => Aggregation.Max(compileTerm(aggregatorVar))
    case SumInt | SumDouble => Aggregation.Sum(compileTerm(aggregatorVar))
    case CountAgg => Aggregation.Count()
    case _ => throw new IllegalArgumentException(s"Unknown aggregation operation: $op")

  private def collectVarRefs(args: Seq[Arg]): Set[String] =
    args.collect {
      case ir.TermArg(v@ir.Var(x)) if v.typ.exists(_.mode.isBinding) => x.name.name
    }.toSet

  private def compileAtom(atom: ir.Atom): Seq[Atom] = atom match {
    case ir.Eq(lhs, rhs, false) => (lhs.typ, rhs.typ) match {
      case (Some(ir.TermType(ty1, m1)), Some(ir.TermType(ty2, m2))) =>
        (m1, m2) match {
          case (Bound, Bound) =>
            Seq(Atom.Equal(compileTerm(lhs, noClone = true), compileTerm(rhs, noClone = true)))
          case (Bound, Binding) =>
            val at = Atom.Let(compileTerm(rhs, noClone = true), compileTerm(lhs, noDeref = true))
            (lhs, rhs) match
              case (ir.Var(n1), ir.Var(n2)) if varRefs.contains(n1.name.name) => varRefs += n2.name.name
              case _ => // nothing
            Seq(at)
          case (Binding, Bound) =>
            val at = Atom.Let(compileTerm(lhs, noClone = true), compileTerm(rhs, noDeref = true))
            (lhs, rhs) match
              case (ir.Var(n1), ir.Var(n2)) if varRefs.contains(n2.name.name) => varRefs += n1.name.name
              case _ => // nothing
            Seq(at)
          case _ =>
            throw new RuntimeException(s"Unexpected binding for terms: $lhs and $rhs")
        }
      case _ => throw new RuntimeException("Untyped terms !")
    }
    case ir.Eq(lhs, rhs, true) =>
      Seq(Atom.NotEqual(compileTerm(rhs), compileTerm(lhs)))
    case arith.BinCompare(lhs, rhs, "<") =>
      Seq(Atom.LesserThan(compileTerm(lhs), compileTerm(rhs)))
    case arith.BinCompare(lhs, rhs, ">") =>
      Seq(Atom.GreaterThan(compileTerm(lhs), compileTerm(rhs)))
    case arith.BinCompare(lhs, rhs, "<=") =>
      Seq(Atom.LesserThanEqual(compileTerm(lhs), compileTerm(rhs)))
    case arith.BinCompare(lhs, rhs, ">=") =>
      Seq(Atom.GreaterThanEqual(compileTerm(lhs), compileTerm(rhs)))
    case ir.Call(name, args, false) =>
      varRefs ++= collectVarRefs(args)
      val argParam = args.map(a => compileArg(a, noDeref = true, noClone = true))
      Seq(Atom.Call(cleanName(name.name), argParam))
    case ir.ExtensionalCall(name, args, false) =>
      varRefs ++= collectVarRefs(args)
      val argParam = args.map(a => compileArg(a, noDeref = true, noClone = true))
      Seq(Atom.Call(cleanName(name.name), argParam))
    case ir.Call(name, args, true) =>
      varRefs ++= collectVarRefs(args)
      val argParam = args.map(a => compileArg(a, noDeref = true, noClone = true))
      Seq(Atom.Not(Atom.Call(cleanName(name.name), argParam)))
    case ir.ExtensionalCall(name, args, true) =>
      varRefs ++= collectVarRefs(args)
      val argParam = args.map(a => compileArg(a, noDeref = true, noClone = true))
      Seq(Atom.Not(Atom.Call(cleanName(name.name), argParam)))
    case data.Deconstruct(t, name, args, neg) =>
      val tmp = freshTmpName()
      val compiledArgs = args.map(p => compileArg(p, noClone = true))
      Seq(Atom.Deconstruct(compileTerm(t, noDeref = true), cleanName(name.name), tmp, compiledArgs, neg))

    case agg.Aggregate(name, args, op) =>
      val Some((agg.AggregateColumnArg(aggColTerm), resultIdx)) = args.zipWithIndex.collectFirst {
        case (col: agg.AggregateColumnArg, idx) => col -> idx
      }
      val aggregatorVar = ir.Var(cleanName(gensym.fresh("agg")))
      val ascentAgg = compileAggOp(op, aggregatorVar)
      val aggArgs = (ascentAgg match
        case Aggregation.Count() => args.patch(resultIdx, Seq(WildcardArg()), 1)
        case _ => args.patch(resultIdx, Seq(TermArg(aggregatorVar)), 1)
      ).map(a => compileArg(a, noClone = true))

      val aggRelName = cleanName(name.name)
      val aggColMode = aggColTerm.typ match
        case Some(ir.TermType(ty, m)) => m
        case _ => throw new RuntimeException("Untyped aggregation output!")

      aggColMode match
        case Binding =>
          val resultName = aggColTerm match
            case v@ir.Var(_) => cleanName(v.name)
            case _ => throw IllegalStateException(s"Found unexpected binding term in aggregation: $aggColTerm")
          Seq(
            Atom.Aggregator(resultName, ascentAgg, Atom.Call(aggRelName, aggArgs))
          )
        case Bound =>
          val tmpName = freshTmpName()
          Seq(
            Atom.Aggregator(tmpName, ascentAgg, Atom.Call(aggRelName, aggArgs)),
            Atom.Equal(Term.DeRef(Term.Var(tmpName)), compileTerm(aggColTerm))
          )
        case Collapse =>
          throw new RuntimeException("Unexpected collapsed term as aggregation output!")
  }

  private def compileArg(arg: ir.Arg, noDeref: Boolean = false, noClone: Boolean = false): Term = arg match
    case AggregateColumnArg(t) => compileTerm(t, noDeref, noClone)
    case ir.TermArg(t) => compileTerm(t, noDeref, noClone)
    case ir.WildcardArg() => Term.Wildcard
    case _ => throw new RuntimeException(s"Unsupported arg: $arg")

  private def compileBinOp(op: String): BinOp = op match
    case "+" => BinOp.Add
    case "-" => BinOp.Sub
    case "*" => BinOp.Mul
    case "/" => BinOp.Div
    case "%" => BinOp.Rem
    case "min" => BinOp.Min
    case "max" => BinOp.Max
    case _ => throw new RuntimeException("Unsupported binary operation: " + op)

  // Function arguments are never dereferenced.
  // We need to clone enums, except when on the lhs of a let.
  // Note, we need clone since enums can not implement copy, because they are boxed
  private def compileTerm(t: ir.Term, noDeref: Boolean = false, noClone: Boolean = false): Term = t match {
    case ir.Var(name) =>
      val varTerm = Term.Var(cleanName(name.name))
      val isRef = varRefs.contains(name.name.name)
      val isData = t.typ.exists(_.ty.isInstanceOf[TData])
      val isString = t.typ.exists(_.ty == TString)
      val derefTerm = if (isRef && !isData && !noDeref)
        Term.DeRef(varTerm)
      else
        varTerm
      if ((isString || isData) && !noClone)
        Term.Clone(derefTerm)
      else
        derefTerm
    case ir.Cast(t, TAny) => compileTerm(t, noDeref)
    case ir.Cast(t, ty) => Term.TypeCast(compileTerm(t, noDeref), compileType(ty))
    case arith.IntNum(n) => Term.NumberLit(n)
    case arith.DoubleNum(n) => Term.FloatLit(n.toFloat)
    case arith.BinOp(lhs, rhs, op) => Term.Binary(compileTerm(lhs), compileBinOp(op), compileTerm(rhs))
    case string.StringLit(s) => Term.StringLit(s)
    case string.ToString(t) => Term.ToString(compileTerm(t, noDeref, noClone))
    case string.StringConcat(t1, t2) => Term.Concat(Seq(compileTerm(t1, true), compileTerm(t2, true)))
    case arith.UnOp(t, "-") => Term.Unary(Unop.neg, compileTerm(t))
    case data.Construct(ref, args) =>
      val caseDef = ref.target.get
      val dataDef = caseDef.data.ref.target.get
      val dataName = cleanName(dataDef.name)
      val compiledArgs = args.map { t =>
        val compiledTerm = compileTerm(t)
        val compiledTy = compileType(t.typ.get.ty, Some(dataName))
        compiledTy match
          case FormatType.Custom(dName, true) => Term.Box(compiledTerm) // Box recursive types
          case FormatType.Symbol => Term.ToString(compiledTerm) // convert strings
          case _ => compiledTerm
      }
      Term.CustomLit(dataName, cleanName(caseDef.name), compiledArgs)
  }

  private def compileType(ty: ir.Type, enclosingDataTypeOption: Option[String] = None): FormatType = ty match
    case TAny => throw IllegalStateException("TAny is not supported by Ascent!")
    case arith.TInt => FormatType.Number
    case arith.TDouble => FormatType.Float
    case string.TString => FormatType.Symbol
    case data.TData(name) =>
      val cleanDataName = cleanName(name.name)
      val needsBoxing = enclosingDataTypeOption match
        case Some(enclosingDataType) => typeDependencies.getOrElse(cleanDataName, Set()).contains(enclosingDataType)
        case _ => false
      FormatType.Custom(cleanDataName, needsBoxing)