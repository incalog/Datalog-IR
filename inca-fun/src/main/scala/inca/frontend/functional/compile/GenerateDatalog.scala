package inca.frontend.functional.compile

import inca.frontend.functional.compile.GenerateDatalog.extensionalRelationName
import inca.frontend.functional.syntax.*
import inca.ir
import inca.ir.{ExtensionalRelation, Language, Name, string2name}
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.block
import inca.ir.extension.bool
import inca.ir.extension.data as irdata
import inca.ir.extension.datamatch as irmatch
import inca.ir.extension.demand
import inca.ir.extension.demand.demandRelationName
import inca.ir.extension.disjunction
import inca.ir.extension.disjunction.DisjunctionAlternative
import inca.ir.extension.not as irnot
import inca.ir.extension.set as irset
import inca.ir.extension.string as irstring
import inca.ir.extension.tuple as irtuple
import inca.util.Gensym
import inca.ir.name2string

object GenerateDatalog:
  def extensionalRelationPrefix = "ext_"
  def extensionalRelationName(name: String) = extensionalRelationPrefix + demandRelationName(name)

class GenerateDatalog {

  val irLang: Language = new Language(Set(ir.BaseIR)
      + irarith.IR + block.IR + bool.IR + irdata.IR + irmatch.IR +
      demand.IR + disjunction.IR + irnot.IR + irset.IR + irstring.IR + irtuple.IR)

  val gensym: Gensym = new Gensym()

  def compileModule(m: Module): ir.Module =
    val mainFunctions = m.content.flatMap {
      case f: FunctionDef if f.annos.exists(_.isInstanceOf[MainFunctionAnno]) => Some(f)
      case _ => None
    }
    val extMainInputRelations = mainFunctions.map { f =>
      val name = extensionalRelationName(f.name)
      val params = f.params.map(p => ir.Param(p.name, compileType(p.typ)))
      ExtensionalRelation(name, params)
    }
    val moduleEntries = m.content.map {
      case f: FunctionDef if f.annos.exists(_.isInstanceOf[MainFunctionAnno]) => compileMainFun(f)
      case f: FunctionDef => compileFun(f)
      case d: DataDef => compileData(d)
    } ++ extMainInputRelations
    ir.Module(m.name, irLang, moduleEntries)

  def compileMainFun(f: FunctionDef): ir.Relation =
    val result = gensym.fresh(f.name.name + "_result")
    val resultParam = ir.Param(Name(result), compileType(f.outType))
    val params = f.params.map(p => ir.Param(p.name, compileType(p.typ))) :+ resultParam
    ir.Relation(f.name, params, Seq(ir.Body(
      Seq(
        ir.ExtensionalCall(extensionalRelationName(f.name), f.params.map(p => ir.Var(p.name))),
        ir.Eq(ir.Var(Name(result)), compileExp(f.body)))
      )
    ))

  def compileFun(f: FunctionDef): ir.Relation =
    val result = gensym.fresh(f.name.name + "_result")
    val resultParam = ir.Param(Name(result), compileType(f.outType))
    val params = f.params.map(p => ir.Param(p.name, demand.TDemand(compileType(p.typ)))) :+ resultParam
    ir.Relation(f.name, params, Seq(ir.Body(
      Seq(ir.Eq(ir.Var(Name(result)), compileExp(f.body))))
    ))

  def compileData(d: DataDef): irdata.DataDefinition =
    irdata.DataDefinition(d.name, d.constrs.map(c => irdata.CaseDefinition(c.name, c.paramTypes.map(compileType))))

  def compileExp(e: Expression): ir.Term = e.cast match
    case None => compileCastedExp(e)
    case Some(trgTy) => ir.Cast(compileCastedExp(e), compileType(trgTy))

  def compileCastedExp(e: Expression): ir.Term = e match
    case Var(name) => ir.Var(name)
    case Let(names, ty, bound, body) =>
      block.Block(
        ir.Eq(irtuple.TupleLit.make(names.map(ir.Var.apply)), compileExp(bound)),
        compileExp(body))
    case If(cnd, thn, els) =>
      val tmp = gensym.fresh("if_result")
      val cndTerm = compileExp(cnd)
      block.Block(
        disjunction.Disjunction(Seq(
          DisjunctionAlternative(ir.Eq(cndTerm, bool.BoolTrue), ir.Eq(ir.Var(Name(tmp)), compileExp(thn))),
          DisjunctionAlternative(ir.Eq(cndTerm, bool.BoolFalse), ir.Eq(ir.Var(Name(tmp)), compileExp(els)))
        )),
        ir.Var(Name(tmp))
      )
    case Call(v@Var(funName), Seq(), args) if v.target.exists(t => t.isInstanceOf[FunctionDef]) =>
      // function call
      val result = gensym.fresh(funName.name + "_call")
      block.Block(
        ir.Call(funName, args.map(compileExp) :+ ir.Var(Name(result))),
        ir.Var(Name(result))
      )
    case Call(v@Var(constrName), Seq(), args) if v.target.exists(t => t.isInstanceOf[DataConstructor]) =>
      // constructor call
      irdata.Construct(constrName, args.map(compileExp))
    case Match(matchee, cases) =>
      val tmp = gensym.fresh("match_result")
      val matcheeTerm = compileExp(matchee)
      val caseAlternatives: Seq[irmatch.Case] = cases.map {
        case (ConstructorPattern(constr, args), body) =>
          irmatch.Case(constr, args.map(a => ir.Var(a.name)),
            Seq(ir.Eq(ir.Var(Name(tmp)), compileExp(body))))
      }
      block.Block(
        irmatch.Match(matcheeTerm, caseAlternatives),
        ir.Var(Name(tmp))
      )

    case BinOp(e1, "==", e2) => bool.AtomAsBool(ir.Eq(compileExp(e1), compileExp(e2)))
    case BinOp(e1, "!=", e2) => bool.AtomAsBool(ir.Neq(compileExp(e1), compileExp(e2)))

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
    case Call(Var(Name("abs")), Seq(), Seq(e1, e2)) => irarith.Abs(compileExp(e1), compileExp(e2))

    case BoolLit(b) => if (b) bool.BoolTrue else bool.BoolFalse
    case BinOp(e1, "&&", e2) => bool.BoolAnd(compileExp(e1), compileExp(e2))
    case BinOp(e1, "||", e2) => bool.BoolOr(compileExp(e1), compileExp(e2))

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
          case t => bool.BoolAtom(t)
        )
      )
    case BinOp(e1, "++", e2) => // set union
      irset.SetUnion(compileExp(e1), compileExp(e2))
    case BinOp(e1, "&", e2) => // set intersection
      irset.SetIntersection(compileExp(e1), compileExp(e2))

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
    case TName(name) => irdata.TData(name)
    case TSet(ty) => irset.TSet(compileType(ty))
    case TFun(_, _) => throw new IllegalArgumentException(s"Must defunctionalize program before compiling")
    case TApply(_, _) => throw new IllegalArgumentException(s"Must monomorph program before compiling")
}
