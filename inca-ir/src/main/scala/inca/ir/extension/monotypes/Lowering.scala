package inca.ir.extension.monotypes

import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Module, Name, Param, Relation, TAny, Term, Type, Var, typing}
import inca.ir.extension.demand
import inca.ir.Hint.preserveHints
import inca.ir.typing.IRTypechecker
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.AggregateArg.{AggregateColumn, Arg}
import inca.ir.extension.aggregate.{Aggregate, AggregateArg}

import scala.collection.mutable


/** Lower mono-types IR into aggregate + demand transformation IR.
 *
 *  Here is the lowering strategy:
 *  - For MkMono(m, args, keys), we do not take any action for now.
 *  - For AddMono(m, input, keys), we substitute it with a
 *    `${Collection}(m, key_1, ..., key_n, input)` relation,
 *    in which [key_1, ..., key_n] = keys, Collection = "Coll$" + input type of
 *    The name of the collection relation is defined as `Coll$` + $.join({type of each key_i}),
 *    because sometimes we would like to send different mono-type instances to one relation.
 *    Besides, each parameter of `Collection` relation is in `TDemand` type.
 *    In the end, we also create a corresponding aggregation rule
 *    `${Aggregation}(m, agg(b)) :- ${Collection}(m, key_1, ..., key_n, input)`,
 *    where Aggregation = Agg${InputTy}${OutputTy}
 *  - For ResultMono(m, b), we substitute it with `Aggregation(m, agg(b))`
 */

trait Lowering extends BaseLowering:
  enum Phase:
    case genColl
    case genAgg

  private var phase: Phase = _

  override def loweredIRs: Set[BaseIR] = Set(IR)

  override def requiredIRs: Set[BaseIR] = Set()

  private val cachedAddMonoCtx: mutable.Map[AddMono, AddMonoInfo] = mutable.Map()

  private val collRelation: mutable.Map[Name, Relation] = mutable.Map()

  private var cachedCollRelation: mutable.Map[Name, Relation] = mutable.Map()

  private val aggRelation: mutable.Map[Name, Relation] = mutable.Map()

  private var cachedAggRelation: mutable.Map[Name, Relation] = mutable.Map()

  override def visitModule(module: Module): Module =
    val typechecker = new IRTypechecker {}
    typechecker.typecheck(module)
    cachedAddMonoCtx ++= typechecker.getAddMonoInfo
    phase = Phase.genColl
    val m1 = super.visitModule(module)
    typechecker.typecheck(m1)
    println("After phase genColl\n" + m1)
    val dmLowering = new demand.Lowering {}
    val m2 = dmLowering.visitProgram(Seq(m1)).head
    typechecker.typecheck(m2)
    println("After phase demand\n" + m2)
    phase = Phase.genAgg
    val m3 = super.visitModule(m2)
    println("phase genAgg\n" + m3)
    typechecker.typecheck(m3)
    m3


  private def genCollName(input: Type, output: Type, keys: Seq[Type]): Name =
    Name(s"Coll$$$input$$$output$$" + keys.mkString("$"))

  private def genAggName(mt: TMono): Name =
    Name(s"Agg$$${mt.input}$$${mt.output}")

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
    collRelation += collName -> collRel
    val aggName = genAggName(mt)
    val aggArgs: Seq[AggregateArg] = collRel.params.dropRight(1).map(t => Arg(Var(t.name))) :+
      AggregateColumn(Var(collRel.params.last.name))
    val aggRel: Relation = Relation(
      aggName,
      Seq(Param(Name("m"), mt), Param(Name("st"), TAny)),
      Seq(Body(Seq(
        Aggregate(collName, aggArgs, ArithmeticMono.CountMono),
        Eq(Var(Name("st")), Var(collRel.params.last.name))
      )))
    )
    aggRelation += aggName -> aggRel
    val collAtom: Atom = Call(collName, mono +: keys :+ input)
    Seq(collAtom)


  private def updateCachedColl() : Unit =
    cachedCollRelation = cachedCollRelation ++ collRelation
    collRelation.clear()

  private def updateCachedAgg(): Unit =
    cachedAggRelation = cachedAggRelation ++ aggRelation
    aggRelation.clear()


  override def visitRelation(relation: Relation): Seq[Relation] = preserveHints(relation) { phase match
    case Phase.genColl =>
      val relations: Seq[Relation] = super.visitRelation(relation) ++ collRelation.map((k, v) => v)
      updateCachedColl()
      relations
    case Phase.genAgg =>
      val relations: Seq[Relation] = super.visitRelation(relation)
         ++ aggRelation.map((k, v) => v).toSeq
      updateCachedAgg()
      relations
  }

//  private def genCollRel(term: MkMono): Seq[Term] =
//    val (in, out) = term.mono.monotypecheck(term.args) match
//      case Left(msg) =>
//        (TAny, TAny)
//      case Right(tm) => tm
//    val mt = TMono(in, out, term.keys)
//    val collName: Name = genCollName(in, out, term.keys)
//    if (!cachedCollRelation.contains(collName)) {
//      val keys: Seq[Param] =
//        for ((ty, i) <- term.keys.zipWithIndex)
//          yield Param(Name("k_" + i), demand.TDemand(ty))
//      val collRel = Relation(
//        collName,
//        Param(Name("m"), demand.TDemand(mt)) +:
//          keys :+ Param(Name("a"), demand.TDemand(in)),
//        Seq(Body(Seq()))
//      )
//      collRelation += collName -> collRel
//    }
//    Seq(term)

//  private def genAggRel(term: MkMono): Seq[Term] =
//    val (in, out) = term.mono.monotypecheck(term.args) match
//      case Left(msg) =>
//        (TAny, TAny)
//      case Right(tm) => tm
//    val mt = TMono(in, out, term.keys)
//    val collName: Name = genCollName(in, out, term.keys)
//    require(cachedCollRelation.contains(collName))
//    val aggName = genAggName(mt)
//    val collRel = cachedCollRelation(collName)
//    val aggArgs : Seq[AggregateArg] = collRel.params.dropRight(1).map(t => Arg(Var(t.name))) :+
//      AggregateColumn(Var(collRel.params.last.name))
//    val aggRel: Relation = Relation(
//      aggName,
//      Seq(Param(Name("m"), mt), Param(Name("st"), TAny)),
//      Seq(Body(Seq(
//        Aggregate(collName, aggArgs, term.mono),
//        Eq(Var(Name("st")), Var(collRel.params.last.name))
//      )))
//    )
//    aggRelation += aggName -> aggRel
//    Seq(term)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term){term match
    case tm@MkMono(mono, args, keys) => Seq(term)
    case tm@ResultMono(m) => Seq(term)
    case _ => super.visitTerm(term)
  }



  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom){atom match
    case atm@AddMono(m, input, keys) => lowerAddMono(atm)
    case _ => super.visitAtom(atom)
  }


