package inca.frontend.functional.compile

import inca.frontend.functional.compile.GenerateIR.extensionalRelationName
import inca.frontend.functional.foreign.FunctionalIncaAggregationOperator
import inca.frontend.functional.syntax.*
import inca.ir
import inca.ir.{ExtensionalRelation, Language, Name, RefByName, TermArg, name2string, string2name}
import inca.ir.extension.aggregate as iragg
import inca.ir.extension.aggregateset as iraggset
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.block
import inca.ir.extension.bool
import inca.ir.extension.data as irdata
import inca.ir.extension.datamatch as irmatch
import inca.ir.extension.demand
import inca.ir.extension.demand.demandRelationName
import inca.ir.extension.disjunction
import inca.ir.extension.disjunction.DisjunctionAlternative
import inca.ir.extension.map as irmap
import inca.ir.extension.not as irnot
import inca.ir.extension.set as irset
import inca.ir.extension.string as irstring
import inca.ir.extension.tuple as irtuple
import inca.ir.extension.tuple.TupleLit
import inca.ir.extension.typeparam
import inca.ir.extension.typeparam.{ParametricModuleEntry, TypeApplication, TypeVar}
import inca.util.Gensym

object GenerateIR:
  def extensionalRelationPrefix = "ext_"
  def extensionalRelationName(name: String): String = extensionalRelationPrefix + demandRelationName(name)

class GenerateIR {

  val irLang: Language = new Language(Set(ir.BaseIR)
      + irarith.IR + block.IR + bool.IR + irdata.IR + irmatch.IR
      + demand.IR + disjunction.IR + irnot.IR + irset.IR + irmap.IR + irstring.IR + irtuple.IR
      + iragg.IR + iraggset.IR + typeparam.IR
  )

  val gensym: Gensym = new Gensym()

  def compileModule(m: Module): ir.Module =
    val mainFunctions = m.content.flatMap {
      case f: FunctionDef if f.annos.exists(_.isInstanceOf[MainFunctionAnno]) => Some(f)
      case _ => None
    }
    val extMainInputRelations = mainFunctions.flatMap { f =>
      val name = extensionalRelationName(f.name)
      val params = f.params.map(p => ir.Param(p.name, compileType(p.typ)))
      if (params.nonEmpty)
        Some(ExtensionalRelation(name, params))
      else
        None
    }
    val moduleEntries = m.content.flatMap {
      case f: FunctionDef if f.annos.exists(_.isInstanceOf[MainFunctionAnno]) => Seq(compileMainFun(f))
      case f: FunctionDef => Seq(compileFun(f))
      case d: DataDef => compileData(d)
    } ++ extMainInputRelations
    ir.Module(m.name, irLang, moduleEntries)

  def compileMainFun(f: FunctionDef): ir.ModuleEntry =
    val result = gensym.fresh(f.name.name + "_result")
    // TODO: How do we handle this case correctly ?
    //   If the main function returns a set there might be no demand on the set relation.
    //   We now force a demand by introducing a SetMember at the end of the main function.
    val returnsSet = f.outType.isInstanceOf[TSet]
    val setMember = if (returnsSet)
      Some(irset.SetMember(ir.Var(Name(gensym.fresh("_"))), ir.Var(Name(result))))
    else
      None
    val resultParam = ir.Param(Name(result), compileType(f.outType))
    val params = f.params.map(p => ir.Param(p.name, compileType(p.typ))) :+ resultParam
    val edbCall =
      if (f.params.nonEmpty)
        Seq(ir.ExtensionalCall(extensionalRelationName(f.name), f.params.map(p => ir.Var(p.name).arg)))
      else
        Seq()
    val rel = ir.Relation(f.name, params, Seq(ir.Body(
      edbCall ++ Seq(
        ir.Eq(ir.Var(Name(result)), compileExp(f.body))
      ) ++ setMember
    )))
    parametric(f.tyVars, rel)

  def compileFun(f: FunctionDef): ir.ModuleEntry =
    val result = gensym.fresh(f.name.name + "_result")
    val resultParam = ir.Param(Name(result), compileType(f.outType))
    val params = f.params.map(p => ir.Param(p.name, demand.TDemand(compileType(p.typ)))) :+ resultParam
    val rel = ir.Relation(f.name, params, Seq(ir.Body(
      Seq(ir.Eq(ir.Var(Name(result)), compileExp(f.body))))
    ))
    parametric(f.tyVars, rel)

  private def parametric(tyVars: Seq[ParametricType], entry: ir.ModuleEntry): ir.ModuleEntry =
    if (tyVars.isEmpty)
      entry
    else
      typeparam.ParametricModuleEntry(tyVars.map(_.name), entry)

  def compileData(d: DataDef): Seq[ir.ModuleEntry] =
    if (d.tyVars.isEmpty) {
      val data = irdata.DataDefinition(d.name)
      val cases = d.constrs.map(c => irdata.CaseDefinition(c.name, c.paramTypes.map(compileType), irdata.TData(data.name)))
      data +: cases
    } else {
      val tyParams = d.tyVars.map(_.name)
      val data = ParametricModuleEntry(tyParams, irdata.DataDefinition(d.name))
      val tdata = irdata.TData(TypeApplication(data.name, tyParams.map(TypeVar.apply)))
      val cases = d.constrs.map(c =>
        ParametricModuleEntry(tyParams, irdata.CaseDefinition(c.name, c.paramTypes.map(compileType), tdata))
      )
      data +: cases
    }

  def compileExp(e: Expression): ir.Term = e.cast match
    case None => compileCastedExp(e)
    case Some(trgTy) => ir.Cast(compileCastedExp(e), compileType(trgTy))

  def compileCastedExp(e: Expression): ir.Term = e match
    case v@Var(name) => v.target match
      case Some(_: FunctionDef) => irmap.MapFrom(name)
      case _ => ir.Var(name)
    case Let(names, ty, bound, body) =>
      block.Block(
        ir.Eq(irtuple.TupleLit.make(names.map(ir.Var.apply)), compileExp(bound)),
        compileExp(body))
    case If(cnd, thn, els) =>
      val tmp = gensym.fresh("if_result")
      val cndTerm = compileExp(cnd)
      block.Block(
        disjunction.Disjunction(Seq(
          DisjunctionAlternative(
            ir.Eq(cndTerm, bool.BoolTrue),
            ir.Eq(ir.Var(Name(tmp)), compileExp(thn))),
          DisjunctionAlternative(
            ir.Eq(cndTerm, bool.BoolFalse),
            ir.Eq(ir.Var(Name(tmp)), compileExp(els)))
        )),
        ir.Var(Name(tmp))
      )
    case Call(v@Var(funName), tyArgs, args) if v.target.exists(t => t.isInstanceOf[FunctionDef]) =>
      // function call
      val result = gensym.fresh(funName.name + "_call")
      val ref: ir.Ref[ir.Relation] = tyArgs match
        case Nil => ir.RefByName(funName)
        case _ => typeparam.TypeApplication(funName, tyArgs.map(compileType))
      block.Block(
        ir.Call(ref, args.map(compileExp).map(_.arg) :+ ir.Var(Name(result)).arg, false),
        ir.Var(Name(result))
      )
    case Call(v@Var(constrName), tyArgs, args) if v.target.exists(t => t.isInstanceOf[DataConstructor]) =>
      // constructor call
      val ref: ir.Ref[irdata.CaseDefinition] = typeparam.TypeApplication.make(constrName, tyArgs.map(compileType))
      irdata.Construct(ref, args.map(compileExp))

    case Match(matchee, cases) =>
      val tmp = gensym.fresh("match_result")
      val matcheeTerm = compileExp(matchee)
      val tyArgs = matchee.typ.get match
        case TApply(_, tyArgs) => tyArgs
        case _ => Seq()

      val caseAlternatives: Seq[irmatch.Case] = cases.map {
        case (pat@ConstructorPattern(constr, args), body) =>
          val (_, data) = pat.target.get
          val ref: ir.Ref[irdata.CaseDefinition] = typeparam.TypeApplication.make(constr, tyArgs.map(compileType))

          irmatch.Case(ref, args.map(a => ir.Var(a.name)),
            Seq(ir.Eq(ir.Var(Name(tmp)), compileExp(body))))
      }
      block.Block(
        irmatch.Match(matcheeTerm, caseAlternatives),
        ir.Var(Name(tmp))
      )

    case BinOp(e1, "==", e2) => bool.AtomAsBool(ir.Eq(compileExp(e1), compileExp(e2)))
    case BinOp(e1, "!=", e2) => bool.AtomAsBool(ir.Eq(compileExp(e1), compileExp(e2), true))

    case StringLit(s) => irstring.StringLit(s)
    case BinOp(e1, "+", e2) if e.typ.contains(TName(Name("String"))) =>
      irstring.StringConcat(compileExp(e1), compileExp(e2))

    case IntLit(i) => irarith.IntNum(i)
    case DoubleLit(d) => irarith.DoubleNum(d)
    case UnOp("-", e) => e.typ match
      case Some(TName(Name("Int"))) => irarith.Sub(irarith.IntNum(0), compileExp(e))
      case Some(TName(Name("Double"))) => irarith.Sub(irarith.DoubleNum(0), compileExp(e))
      case _ => throw new IllegalArgumentException(s"Cannot compile code of type ${e.typ}, $e")
    case BinOp(e1, "+", e2) => irarith.Add(compileExp(e1), compileExp(e2))
    case BinOp(e1, "*", e2) => irarith.Mul(compileExp(e1), compileExp(e2))
    case BinOp(e1, "-", e2) => irarith.Sub(compileExp(e1), compileExp(e2))
    case BinOp(e1, "/", e2) => irarith.Div(compileExp(e1), compileExp(e2))
    case BinOp(e1, "%", e2) => irarith.Remainder(compileExp(e1), compileExp(e2))
    case BinOp(e1, ">", e2) => bool.AtomAsBool(irarith.GT(compileExp(e1), compileExp(e2)))
    case BinOp(e1, ">=", e2) => bool.AtomAsBool(irarith.GE(compileExp(e1), compileExp(e2)))
    case BinOp(e1, "<", e2) => bool.AtomAsBool(irarith.LT(compileExp(e1), compileExp(e2)))
    case BinOp(e1, "<=", e2) => bool.AtomAsBool(irarith.LE(compileExp(e1), compileExp(e2)))
    case Call(Var(Name("min")), Seq(), Seq(e1, e2)) => irarith.Min(compileExp(e1), compileExp(e2))
    case Call(Var(Name("max")), Seq(), Seq(e1, e2)) => irarith.Max(compileExp(e1), compileExp(e2))
    case Call(Var(Name("abs")), Seq(), Seq(e)) => irarith.Abs(compileExp(e))

    case BoolLit(b) => if (b) bool.BoolTrue else bool.BoolFalse
    case BinOp(e1, "&&", e2) => bool.BoolAnd(compileExp(e1), compileExp(e2))
    case BinOp(e1, "||", e2) => bool.BoolOr(compileExp(e1), compileExp(e2))

    case Lambda(vs, body) =>
      irmap.MapFun(vs.map(p => ir.Param(p._1, compileType(p._2))), compileExp(body))

    case Call(fun, Seq(), args) =>
      // function-value call
      val map = compileExp(fun)
      irmap.MapLookUp(map, TupleLit.make(args.map(compileExp)))
    case Tuple(es) => irtuple.TupleLit.make(es.map(compileExp))

    case SetExp(es) => irset.SetLit(es.map(compileExp))
    case SetMember(tup, set, neg) =>
      val memTerm = irset.SetMember(compileExp(tup), compileExp(set))
      val test = if (neg)
        irnot.Not(memTerm)
      else
        memTerm
      bool.AtomAsBool(memTerm)
    case SetComprehension(build, predicates) =>
      irset.SetComprehension(compileExp(build),
        predicates.map(p => compileExp(p) match
          case bool.AtomAsBool(at) => at
          case t => ir.Eq(t, bool.BoolTrue)
        )
      )
    case BinOp(e1, "++", e2) => // set union
      irset.SetUnion(compileExp(e1), compileExp(e2))
    case BinOp(e1, "&", e2) => // set intersection
      irset.SetIntersection(compileExp(e1), compileExp(e2))
    case SetFold(anno, init, op@Var(opname), Call(Var(name), Seq(), args)) =>
      /* For each fold(init, op, set) the following holds
       *  1. set == Call(Var(setName), setArgs) for some setName and setArgs
       *  2. the fold construct occurs in its own function as to avoid duplicate aggregation
       */
      val f = op.target match
        case Some(f: FunctionDef) => f
        case trg => throw new IllegalArgumentException(s"Cannot compile fold with non-function op target $trg")
      val aggOp = FunctionalIncaAggregationOperator(f, init, op)

      val aggResult = Name(gensym.fresh("foldResult"))
      val argTerms = args.map(compileExp)
      val aggArgs = argTerms.map(TermArg.apply) :+
        iragg.AggregateColumnArg(ir.Var(aggResult))
      val agg = iraggset.AggregateSet(ir.RefByName(name), aggArgs, aggOp)
      block.Block(Seq(agg), ir.Var(aggResult))

    case _ =>
      throw new IllegalArgumentException(s"Cannot compile $e")

  def compileType(ty: Type): ir.Type = ty match
    case TAny => ir.TAny
    case TNothing => ir.TNothing
    case TTuple(ts) => irtuple.TTuple(ts.map(compileType))
    case TName(Name("Int")) => irarith.TInt
    case TName(Name("Double")) => irarith.TDouble
    case TName(Name("Boolean")) => bool.TBoolean
    case TName(Name("String")) => irstring.TString
    case ty@TName(name) => ty.target.get match
      case _: DataDef => irdata.TData(name)
      case _: ParametricType => typeparam.TypeVar(name)
    case TSet(ty) => irset.TSet(compileType(ty))
    case TFun(from, to) =>
      val inputs = from.map(compileType)
      val output = compileType(to)
      irmap.TMap(irtuple.TTuple.make(inputs), output)
    case TApply(named@TName(name), args) => named.target match
      case Some(_: DataDef) => irdata.TData(TypeApplication(name, args.map(compileType)))
      case Some(_) => throw new IllegalArgumentException(s"Cannot compile type application $ty because $name is not a data type")
      case None => throw new IllegalArgumentException(s"Unresolved name $name in $ty")
}
