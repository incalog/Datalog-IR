package inca.ir.extension.monotypes

import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Module, Name, Param, Relation, TAny, Term, Type, Var, typing}
import inca.ir.extension.demand
import inca.ir.Hint.preserveHints
import inca.ir.typing.IRTypechecker
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.AggregateArg.{AggregateColumn, Arg}
import inca.ir.extension.aggregate.{Aggregate, AggregateArg}
import inca.ir.extension.block

import scala.collection.mutable


trait Lowering extends BaseLowering:

  // only generate Coll and Agg relation when met ResultMono
  enum Phase:
    case First // lower MkMono and AddMono, generate Agg relation but not
              // return them
    case Second // lower ResultMono and release Agg
    //TODO: case Third // remove all of the AddMono

  private var phase: Phase = _

  override def loweredIRs: Set[BaseIR] = Set(IR)

  override def requiredIRs: Set[BaseIR] = Set()

  private val cachedAddMonoCtx: mutable.Map[AddMono, AddMonoInfo] = mutable.Map()

  private val cachedResultMonoCtx: mutable.Map[ResultMono, TMono] = mutable.Map()

  private val collRelation: mutable.Map[Name, Relation] = mutable.Map()

  private var cachedCollRelation: mutable.Map[Name, Relation] = mutable.Map()

  private val aggRelation: mutable.Map[Name, Relation] = mutable.Map()

  private var cachedAggRelation: mutable.Map[Name, Relation] = mutable.Map()

  private var aggCollRelation: mutable.Map[Name, Relation] = mutable.Map()

  private var cachedAggCollRelation: mutable.Map[Name, Relation] = mutable.Map()


  override def visitModule(module: Module): Module =
    val typechecker = new IRTypechecker {}
    typechecker.typecheck(module)
    cachedAddMonoCtx ++= typechecker.getAddMonoInfo
    cachedResultMonoCtx ++= typechecker.getResultMonoInfo
    phase = Phase.First
    val m1: Module = super.visitModule(module)
    typechecker.typecheck(m1)
    val dmLowering = new demand.Lowering {}
    val m2: Module = dmLowering.visitProgram(Seq(m1)).head
    typechecker.typecheck(m2)
    phase = Phase.Second
    val m3: Module = super.visitModule(m2)
    println(m3)
    typechecker.typecheck(m3)
    m3

  private def genCollName(input: Type, output: Type, keys: Seq[Type]): Name =
    Name(s"Coll$$$input$$$output$$" + keys.mkString("$"))

  private def genAggName(mt: TMono): Name =
    Name(s"Agg$$${mt.input}$$${mt.output}")


  private def genAggCollName(mt: TMono): Name =
    Name(s"AggColl$$${mt.input}$$${mt.output}")


  protected def lowerAddMono(atom: AddMono) : Seq[Atom] =
    val mono: Term = atom.m
    val input: Term = atom.input
    val keys: Seq[Term] = atom.keys
    require(cachedAddMonoCtx.contains(atom))
    val info: AddMonoInfo = cachedAddMonoCtx(atom)
    val mt = TMono(info.monoTy.asInstanceOf[TMono].input, info.monoTy.asInstanceOf[TMono].output, info.keysTy)
    val collName: Name = genCollName(mt.input, mt.output, mt.keys)
    val params: Seq[Param] =
      for ((ty, i) <- info.keysTy.zipWithIndex)
        yield Param(Name("k_" + i), demand.TDemand(ty))
    val collRel = Relation(
      collName,
      Param(Name("m"), demand.TDemand(mt)) +:
        params :+ Param(Name("a"), demand.TDemand(mt.input)),
      Seq(Body(Seq()))
    )
    val collAtom: Atom = Call(collName, mono +: keys :+ input)
    Seq(collAtom)


  private def updateCachedColl() : Unit =
    cachedCollRelation = cachedCollRelation ++ collRelation
    collRelation.clear()

  private def updateCachedAgg(): Unit =
    cachedAggRelation = cachedAggRelation ++ aggRelation
    aggRelation.clear()

  private def updateCachedAggColl(): Unit =
    cachedAggCollRelation = cachedAggCollRelation ++ aggCollRelation
    aggCollRelation.clear()


  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) {
    phase match
      case Phase.First =>
        val relations: Seq[Relation] = super.visitRelation(relation) ++ collRelation.map((k, v) => v)
        updateCachedColl()
        relations
      case Phase.Second =>
        val relations = super.visitRelation(relation)
            ++ aggRelation.map((k, v) => v)
            ++ aggCollRelation.map((k, v) => v)
        updateCachedAgg()
        updateCachedAggColl()
        relations
  }


  // Generate the corresponding Coll relation and Agg relation regarding
  // the given MkMono definition.
  private def lowerMkMono(term:MkMono) : Seq[Term] =
    val (in, out): (Type, Type) = term.mono.monotypecheck(term.args) match
      case Left(msg) =>
        (TAny, TAny)
      case Right(tm) => tm
    val mt: TMono = TMono(in, out, term.keys)
    val collName: Name = genCollName(in, out, term.keys)
    if (!cachedCollRelation.contains(collName)) {
      val keys: Seq[Param] =
        for ((ty, i) <- term.keys.zipWithIndex)
          yield Param(Name("k_" + i), demand.TDemand(ty))
      val collRel = Relation(
        collName,
        Param(Name("m"), demand.TDemand(mt)) +:
          keys :+ Param(Name("a"), demand.TDemand(in)),
        Seq(Body(Seq()))
      )
      collRelation += collName -> collRel
      val call: Call = Call(collName, collRel.params.map(p => Var(p.name)))
      val aggCollName = genAggCollName(mt)
      val aggCollRel: Relation = Relation(
        aggCollName,
        Seq(Param(Name("m"), mt), Param(Name("a"), in)),
        Seq(Body(Seq(
          call
        )))
      )
      aggCollRelation += aggCollName -> aggCollRel
      val aggName = genAggName(mt)
      val aggArgs: Seq[AggregateArg] = aggCollRel.params.dropRight(1).map(t => Arg(Var(t.name))) :+
        AggregateColumn(Var(Name("b")))
      val aggCall: Call = Call(aggCollName, aggCollRel.params.map(p => Var(p.name)))
      val aggRel: Relation = Relation(
        aggName,
        Seq(Param(Name("m"), demand.TDemand(mt)), Param(Name("b"), out)),
        Seq(Body(Seq(
          Aggregate(aggCollName, aggArgs, term.mono)
        )))
      )
      aggRelation += aggName -> aggRel
    }

    Seq(term)


  private def lowerResultMono(term: ResultMono): Seq[Term] =
    require(cachedResultMonoCtx.contains(term))
    val ty: TMono = cachedResultMonoCtx(term)
    val aggName: Name = genAggName(ty)
    val b: Term = Var(Name(term.m.asInstanceOf[Var].name.toString + "r"))
    val call: Call = Call(aggName, Seq(term.m, b))
    Seq(block.Block(Seq(call), b))



  override def visitTerm(term: Term): Seq[Term] = preserveHints(term){term match
    case MkMono(mono, args, keys) => lowerMkMono(MkMono(mono, args, keys))
    case ResultMono(m) => phase match
      case Phase.First => Seq(ResultMono(m))
      case _ => lowerResultMono(ResultMono(m))
    case _ => super.visitTerm(term)
  }



  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom){atom match
    case AddMono(m, input, keys) => lowerAddMono(AddMono(m, input, keys))
    case _ => super.visitAtom(atom)
  }


