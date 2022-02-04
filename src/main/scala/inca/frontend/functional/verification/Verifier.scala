package inca.frontend.functional.verification

import inca.frontend.functional.core.{Call, Let, Match, _}
import smtlib.interpreters.Z3Interpreter
import smtlib.trees.Commands.Script

import scala.collection.mutable
//import smtlib.theories.Ints._
//import smtlib.trees.Terms._
//import smtlib.trees.Commands._


// Functional Program
// Collect functions with verification annotations IncA
// get provable properties IncA
// collect datatype definitions
// compile data types to smt lib
// compile function to smt lib (IncA -> SMTLIB)
// Foreach provable property add provable goal (assertion) (SMTLIB -> SMTLIB)
// execute z3 with smtlib as input
// SMTLIB


class Verifier {

  def module: Module = ???

  val functionDict: mutable.Map[Name, FunctionDef] = mutable.Map()
  val dataDict: mutable.Map[Name, DataDef] = mutable.Map()

  def verify(module: Module): Unit = {
    fillDicts(module)
    val aggregations: Map[Name, Seq[Property]] = collectAggregations(module)
    val verificationScripts: Seq[Script] = aggregations.toSeq.map(ag => generate(ag._1, ag._2))
    implicit val z3Interp: Z3Interpreter = Z3Interpreter.buildDefault
    verificationScripts.foreach(s =>
      smtlib.Interpreter.execute(s)
    )
  }

  def fillDicts(module: Module): Unit = module.content.foreach {
    case d: DataDef => dataDict += d.name -> d
    case f: FunctionDef => functionDict += f.name -> f
  }
  def collectAggregations(module: Module): Map[Name, Seq[Property]] = ???

  def generate(funcName: Name, props: Seq[Property]): Script = {
    val calledFunctions = collectCalledFunctions(functionDict(funcName).body).toSeq
    val dataDefs = calledFunctions.flatMap(fName => collectUsedDataDefs(functionDict(fName))).toSeq
    val transDataDefs =  dataDefs.map(transDataDef)
    val transFuncDefs = calledFunctions.map(transFunctionDef)
    val transProps = props.map(transProperty)
    makeScript(transDataDefs ++ transFuncDefs ++ transProps)
  }

  def collectCalledFunctions(funBody: Expression): Set[Name] = {
    funBody match {
      case Var(name) => Set()
      case inca.frontend.functional.core.Let(names, anno, bound, body) =>
        collectCalledFunctions(bound) ++ collectCalledFunctions(body)
      case BaseApplyInfix(left, op, right) =>
        collectCalledFunctions(left)++collectCalledFunctions(right)
      case BaseLit(code) => Set()
      case If(cnd, thn, els) =>
        collectCalledFunctions(cnd)++collectCalledFunctions(thn)++collectCalledFunctions(els)
      case Match(matchee: Expression, cases: Seq[(Pattern, Expression)]) =>
        collectCalledFunctions(matchee)++cases.flatMap(c => collectCalledFunctions(c._2))
      case Call(fun, args, transitive) => fun match {
        case Var(name) => Set(name)++args.flatMap(collectCalledFunctions)
        case _ => collectCalledFunctions(fun)++args.flatMap(collectCalledFunctions)
      }
      case BaseApply(fun, args) =>
        args.flatMap(collectCalledFunctions).toSet
      case Tuple(exps) =>
        exps.flatMap(collectCalledFunctions).toSet
      case SetExp(es) =>
        es.flatMap(collectCalledFunctions).toSet
      case SetComprehension(build, predicates) =>
        collectCalledFunctions(build)++predicates.flatMap(collectCalledFunctions)
      case SetMember(tup, set, neg) =>
        collectCalledFunctions(tup)++collectCalledFunctions(set)
      case Lambda(vs, body) => collectCalledFunctions(body)
      case SetFold(anno, init, op, set) =>
        collectCalledFunctions(init)++collectCalledFunctions(op)++collectCalledFunctions(set)
      case SomeExp(e) => collectCalledFunctions(e)
      case NoneExp() => Set()
    }
  }

  def collectUsedDataDefs(func: FunctionDef): Set[Name] = {
    val funcTypes = (Set(func.outType)++func.params.map(p => p.typ)).flatMap(getDataTypes)
    funcTypes ++ funcTypes.flatMap(dataName => {
      val constrs = dataDict(dataName).constrs
      constrs.flatMap(c => c.paramTypes.flatMap(getDataTypes))
    })
  }

  def getDataTypes(outType: Type): Set[Name] = {
    outType match {
      case TFun(from, to) => (from.flatMap(getDataTypes)++getDataTypes(to)).toSet
      case TTuple(ts) => ts.flatMap(getDataTypes).toSet
      case TData(name) => Set(name)
      case TOption(ty) => getDataTypes(ty)
      case TSet(ty) => getDataTypes(ty)
      case _ => Set()
    }
  }

  def transDataDef(dataName: Name): Script = ???

  def transFunctionDef(func: Name): Script = ???

  def transProperty(prop: Property): Script = ???
    //prop match {
    //case Assoc => transAsssoc(name)
    //case Commutativity => transCommu(name)


  def makeScript(scripts: Seq[Script]): Script = ???

  case class Property()

}

// implicit val z3Interp = Z3Interepreter.buildDefault
// val script = ..
// Interpreter.execute(script)

// def xyz(script: Script)(implicit interp: Interpreter): ... = {
//   Interpreter.execute(script)
// }

// class X(x: Int)
// val o = new X(1)

// case class X(x: Int)
// val o = X(1)
// val o = X.apply(1)

// generated
// object X {
//   def apply(x: Int): X = new X(x)
//   def unapply(x: X): Option[Int]) = ...
// }
