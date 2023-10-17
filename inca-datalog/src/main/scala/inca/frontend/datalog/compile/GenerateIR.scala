package inca.frontend.datalog.compile

import inca.ir
import inca.ir.extension.aggregate
import inca.ir.extension.arithmetic
import inca.ir.extension.string
import inca.frontend.datalog.syntax.*
import inca.ir.{Language, Name}
import inca.util.Gensym

class GenerateIR {

  val gensym: Gensym = new Gensym()

  val irLang: Language = new Language(Set(ir.BaseIR) + arithmetic.IR + string.IR)

  def compileModule(m: Module): ir.Module =
    ir.Module(Name("Datalog"), irLang, m.relations.flatMap(compileRelation))

  def compileRelation(r: Relation): Seq[ir.Relation] = gensym.scoped {
    val vars = r.params.map(ty => Name(gensym.fresh("param")) -> compileType(ty))
    val params = vars.map(v => ir.Param(v._1, v._2))
    val bodies = r.rules.map(compileRule(_, vars.map(_._1)))

    val aggregationParams = r.rules.map(_.head.indexWhere(_.isInstanceOf[Param.Aggregated])).distinct
    if (aggregationParams.size != 1) {
      throw new IllegalArgumentException(s"Conflicting aggregation annotations in $r")
    } else if (aggregationParams.head == -1) {
      Seq(ir.Relation(r.name, params, bodies))
    } else {
      val collectName = Name(gensym.fresh(r.name.name + "$Collect"))
      val collectRel = ir.Relation(collectName, params, bodies)

      val aggregateIndex = aggregationParams.head
      val aggregateParam = r.rules.head.head(aggregateIndex).asInstanceOf[Param.Aggregated]

      val aggOp = compileAggregationOperator(aggregateParam.agg.name)
      val args = vars.map(v => aggregate.AggregateArg.Arg(ir.Var(v._1)))
      val aggArgs = args.updated(aggregateIndex,
        aggregate.AggregateArg.AggregateColumn(ir.Var(vars(aggregateIndex)._1)))
      val aggAtom = aggregate.Aggregate(collectName, aggArgs, aggOp)

      val collectArgs = vars.map(_._1)
        .updated(aggregateIndex, Name(gensym.fresh("dummy")))
        .map(ir.Var.apply)
      val collectAtom = ir.Call(collectName, collectArgs)

      val aggRel = ir.Relation(r.name, params, Seq(ir.Body(Seq(
        collectAtom,
        aggAtom
      ))))
      Seq(collectRel, aggRel)
    }
  }

  def compileAggregationOperator(name: String): aggregate.AggregationOperator = name match
    case "count" => arithmetic.ArithmeticAggregationOperator.Count
    case "sum" => arithmetic.ArithmeticAggregationOperator.Sum
    case "min" => arithmetic.ArithmeticAggregationOperator.Min
    case "max" => arithmetic.ArithmeticAggregationOperator.Max

  def compileRule(r: Rule, vars: Seq[Name]): ir.Body =
    val headAtoms = r.head.zip(vars) map {
      case (Param.Constant(l), x) => ir.Eq(ir.Var(x), compileTerm(Term.Constant(l)))
      case (Param.Named(n), x) => ir.Eq(ir.Var(x), ir.Var(n))
      case (Param.Aggregated(n, _), x) => ir.Eq(ir.Var(x), ir.Var(n))
    }
    ir.Body(r.body.map(compileAtom) ++ headAtoms)

  def compileAtom(a: Atom): ir.Atom = a match
    case Atom.Call(name, args, false) => ir.Call(name, args.map(compileTerm))
    case Atom.Call(name, args, true) => ir.NegCall(name, args.map(compileTerm))
    case Atom.Compare(lhs, op, rhs) => op match
      case "==" => ir.Eq(compileTerm(lhs), compileTerm(rhs))
      case "!=" => ir.Neq(compileTerm(lhs), compileTerm(rhs))
      case _ => arithmetic.BinCompare(compileTerm(lhs), compileTerm(rhs), op)

  def compileTerm(t: Term): ir.Term = t match
    case Term.Var(name) => ir.Var(name)
    case Term.Constant(lit) => lit match
      case Literal.Int(i) => arithmetic.IntNum(i)
      case Literal.Double(d) => arithmetic.DoubleNum(d)
      case Literal.String(s) => string.StringLit(s)
    case Term.BinOp(lhs, op, rhs) => t.typ.get match
      case Type.Int() | Type.Double() =>
        arithmetic.BinOp(compileTerm(lhs), compileTerm(rhs), op)
      case Type.String() =>
        if (op == "+")
          string.StringConcat(compileTerm(lhs), compileTerm(rhs))
        else
          throw new IllegalArgumentException(t.toString)

  def compileType(ty: Type): ir.Type = ty match
    case Type.Int() => arithmetic.TInt
    case Type.Double() => arithmetic.TDouble
    case Type.String() => string.TString
}
