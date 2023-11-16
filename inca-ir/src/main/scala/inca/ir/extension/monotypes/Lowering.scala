package inca.ir.extension.monotypes

import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Module, ModuleEntry, Name, Param, Relation, TAny, Term, Type, Var, typing}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.demand
import inca.ir.Hint.preserveHints
import inca.ir.typing.IRTypechecker
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.AggregateArg.{AggregateColumn, Arg, WildCard}
import inca.ir.extension.aggregate.{Aggregate, AggregateArg}
import inca.ir.extension.block
import inca.ir.extension.demand.Hints.IgnoreCall
import inca.ir.extension.impure.Impure
import inca.ir.extension.data
import inca.ir.extension.data.{Construct, DataDefinition, CaseDefinition, TData, Deconstruct}
import inca.ir.extension.arithmetic.{Add, IntNum, TInt}
import inca.ir.extension.string.{StringLit, TString}

import scala.collection.mutable


trait Lowering extends BaseLowering:

  private val debug: Boolean = true

  override def loweredIRs: Set[BaseIR] = Set(IR)

  override def requiredIRs: Set[BaseIR] = Set()

  private val cachedAddMonoCtx: mutable.Map[AddMono, AddMonoInfo] = mutable.Map()

  private val cachedResultMonoCtx: mutable.Map[ResultMono, TMono] = mutable.Map()

  private val cachedMkMonoCtx: mutable.Map[MkMono, Seq[Type]] = mutable.Map()

  private val cachedRelation: mutable.Map[Name, Relation] = mutable.Map()

  private val monoData: DataDefinition = DataDefinition(
    Name("Mono"), Seq(CaseDefinition(Name("Mono"), Seq(TInt, TString)))
  )



  override def visitModule(module: Module): Module =
    val typechecker = new IRTypechecker {}
    typechecker.typecheck(module)
    cachedAddMonoCtx ++= typechecker.getAddMonoInfo
    cachedResultMonoCtx ++= typechecker.getResultMonoInfo
    val m1: Module = super.visitModule(module)
    val m2: Module = Module(
      m1.name,
      m1.lang,
      monoData +: (m1.contents ++ cachedRelation.map((k, v) => v))
    )
    if debug then println("Before demand lowering\n" + m2)
    val dmLowering = new demand.Lowering {}
    val m3: Module = dmLowering.visitProgram(Seq(m2)).head
    if debug then println("After demand lowering, program becomes\n" + m3)
    typechecker.typecheck(m3)
    if debug then println("Type checking successful\n\n")
    m3

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry) {
    def lowerTMono(ty: Type): Type =
      ty match
        case TMono(_, _, _) => TData(Name("Mono"))
        case TDemand(dt) => dt match
          case TMono(_, _, _) => TDemand(TData(Name("Mono")))
          case _ => ty
        case _ => ty

    moduleEntry match
      case Relation(name, params, bodies) =>
        super.visitModuleEntry(Relation(name, params map {case Param(name, ty) => Param(name, lowerTMono(ty))}, bodies))
      case _ => super.visitModuleEntry(moduleEntry)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case AddMono(m, input, keys) => lowerAddMono(AddMono(m, input, keys))
      case Eq(lhs, rhs) => (lhs, rhs) match
        case (Var(v), MkMono(mono, args, keys)) => lowerMkMono(Var(v), MkMono(mono, args, keys))
        case _ => super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case ResultMono(m) => lowerResultMono(ResultMono(m))
      case _ => super.visitTerm(term)
  }

  private def genCollName(mt: TMono): Name =
    Name(s"Coll$$${mt.input}$$${mt.output}$$" + mt.keys.mkString("$"))

  private def genAggName(mt: TMono): Name =
    Name(s"Agg$$${mt.input}$$${mt.output}")

  protected def lowerAddMono(atom: AddMono) : Seq[Atom] =
    val mono: Term = atom.m
    val input: Term = atom.input
    val keys: Seq[Term] = atom.keys
    require(cachedAddMonoCtx.contains(atom))
    val info: AddMonoInfo = cachedAddMonoCtx(atom)
    val mt = TMono(info.monoTy.asInstanceOf[TMono].input, info.monoTy.asInstanceOf[TMono].output, info.keysTy)
    val collName: Name = genCollName(mt)
    if (!cachedRelation.contains(collName)) {
      val keysParam: Seq[Param] =
        for ((ty, i) <- mt.keys.zipWithIndex)
          yield Param(Name("k_" + i), TDemand(ty))
      val collRel = Relation(
        collName,
        Param(Name("m"), TDemand(TData(Name("Mono")))) +:
          keysParam :+ Param(Name("a"), TDemand(mt.input)),
          Seq(Body(Seq()))
      )
      cachedRelation += collName -> collRel
    }
    val collAtom: Atom = Call(collName, mono +: keys :+ input)
    Seq(collAtom)

  private def lowerMkMono(v: Var, term:MkMono) : Seq[Atom] =
    val (in, out): (Type, Type) = term.mono.monotypecheck(term.args) match
      case Left(msg) =>
        (TAny, TAny)
      case Right(tm) => tm
    val mt: TMono = TMono(in, out, term.keys)
    val collName: Name = genCollName(mt)
    if (!cachedRelation.contains(collName)) {
      val keys: Seq[Param] =
        for ((ty, i) <- term.keys.zipWithIndex)
          yield Param(Name("k_" + i), TDemand(ty))
      val collRel = Relation(
        collName,
        Param(Name("m"), TDemand(TData(Name("Mono")))) +:
          keys :+ Param(Name("a"), TDemand(in)),
        Seq(Body(Seq()))
      )
      cachedRelation += collName -> collRel
    }
    val aggName = genAggName(mt)
    val wildCardArgs = mt.keys.map(_ => WildCard)
    val aggArgs: Seq[AggregateArg] = Arg(Var(Name("m"))) +: wildCardArgs :+
      AggregateColumn(Var(Name("b")))
    // TODO: an aggregate operator should be able to receive initializing arguments
    val aggAtom: Aggregate = Aggregate(collName, aggArgs, term.mono)
    aggAtom.addHint(IgnoreCall)
    val aggRel: Relation = Relation(
      aggName,
      Seq(Param(Name("m"), TDemand(TData(Name("Mono")))), Param(Name("b"), out)),
      Seq(Body(Seq(
        Deconstruct(
          Var(Name("m")),
          Name("Mono"),
          Seq(
            Var(Name("id")),
            Var(Name("name"))
          )),
        Eq(Var(Name("name")), StringLit(term.mono.toString)),
        aggAtom
    ))))
    if (!cachedRelation.contains(aggName)){
      cachedRelation += aggName -> aggRel
    } else {
      val cachedAgg = cachedRelation(aggName)
      cachedRelation(aggName) = Relation(
        cachedAgg.name,
        cachedAgg.params,
        cachedAgg.bodies ++ aggRel.bodies
      )
    }

    val state = Var(Name(gensym.fresh("st")))
    val monoDefId = term.mono.toString
    val mono: Construct = Construct(Name("Mono"), Seq(state, StringLit(monoDefId)))
    val imp: Impure = Impure(
      state,
      Seq(Eq(v, mono)),
      Add(state, IntNum(1)),
      MonoImpurityKind
    )
    Seq(imp)


  private def lowerResultMono(term: ResultMono): Seq[Term] =
    require(cachedResultMonoCtx.contains(term))
    val ty: TMono = cachedResultMonoCtx(term)
    val aggName: Name = genAggName(ty)
    val mvar: Var = Var(term.m.asInstanceOf[Var].name)
    val b: Term = Var(Name(mvar.toString + "r"))
    val call: Call = Call(aggName, Seq(mvar, b))
    Seq(block.Block(Seq(call), b))
