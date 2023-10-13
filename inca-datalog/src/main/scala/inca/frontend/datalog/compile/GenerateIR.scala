package inca.frontend.datalog.compile

import inca.ir
import inca.ir.extension.arithmetic
import inca.ir.extension.string
import inca.frontend.datalog.syntax.*
import inca.ir.{Language, Name}
import inca.util.Gensym

import javax.naming.OperationNotSupportedException

class GenerateIR {

  val gensym: Gensym = new Gensym()

  val irLang: Language = new Language(Set(ir.BaseIR) + arithmetic.IR + string.IR)

  def compileModule(m: Module): ir.Module =
    ir.Module(Name("Datalog"), irLang, m.relations.map(compileRelation))

  def compileRelation(r: Relation): ir.Relation =
    val vars = r.params.map(ty => Name(gensym.fresh(ty.toString)) -> compileType(ty))
    val params = vars.map(v => ir.Param(v._1, v._2))
    val bodies = r.rules.map(compileRule(_, vars.map(_._1)))
    ir.Relation(r.name, params, bodies)

  def compileRule(r: Rule, vars: Seq[Name]): ir.Body =
    val headAtoms = r.head.zip(vars) map {
      case (Param.Constant(l), x) => ir.Eq(ir.Var(x), compileTerm(Term.Constant(l)))
      case (Param.Named(n), x) => ir.Eq(ir.Var(x), ir.Var(n))
      case (Param.Aggregated(n, agg), x) => throw new OperationNotSupportedException()
    }
    ir.Body(r.body.map(compileAtom))

  def compileAtom(a: Atom): ir.Atom = a match
    case Atom.Call(name, args, true) => ir.Call(name, args.map(compileTerm))
    case Atom.Call(name, args, false) => ir.NegCall(name, args.map(compileTerm))
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
