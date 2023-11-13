package inca.ir.extension.monotypes

import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Module, Name, Neq, Param, Relation, TAny, Term, Type, Var, typing}
import inca.ir.extension.demand
import inca.ir.Hint.preserveHints
import inca.ir.typing.IRTypechecker
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.AggregateArg.{AggregateColumn, Arg, WildCard}
import inca.ir.extension.aggregate.{Aggregate, AggregateArg}
import inca.ir.extension.block
import inca.ir.extension.demand.Hints.IgnoreCall
import inca.ir.extension.data.Construct

import scala.collection.mutable


trait Lowering extends BaseLowering:

  // only generate Coll and Agg relation when met ResultMono
  private val debug: Boolean = false

  override def loweredIRs: Set[BaseIR] = Set(IR)

  override def requiredIRs: Set[BaseIR] = Set()

  private val cachedAddMonoCtx: mutable.Map[AddMono, AddMonoInfo] = mutable.Map()

  private val cachedResultMonoCtx: mutable.Map[ResultMono, TMono] = mutable.Map()

  private val cachedCollRelation: mutable.Map[Name, Relation] = mutable.Map()

  private val cachedAggRelation: mutable.Map[Name, Relation] = mutable.Map()


  override def visitModule(module: Module): Module =
    val typechecker = new IRTypechecker {}
    typechecker.typecheck(module)
    cachedAddMonoCtx ++= typechecker.getAddMonoInfo
    cachedResultMonoCtx ++= typechecker.getResultMonoInfo
    val m1: Module = super.visitModule(module)
    val m2: Module = Module(m1.name, m1.lang, m1.contents ++ cachedCollRelation.map((k, v) => v) ++ cachedAggRelation.map((k, v) => v))
    if debug then
      println("After the first phase before type checking\n" + m2)
      println()
    typechecker.typecheck(m2)
    if debug then
      println("Type checking successful\n\n")
    val dmLowering = new demand.Lowering {}
    val m3: Module = dmLowering.visitProgram(Seq(m2)).head
    if debug then
      println("After demand lowering, program becomes\n" + m3)
    typechecker.typecheck(m3)
    if debug then println("Type checking successful\n\n")
    m3

  private def genCollName(mt: TMono): Name =
    Name(s"Coll$$${mt.input}$$${mt.output}$$" + mt.keys.mkString("$"))

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
    val collName: Name = genCollName(mt)
    if (cachedCollRelation.contains(collName)) {
      val auxCollRel = cachedCollRelation(collName)
      var availableBodies: Set[Body] = auxCollRel.bodies.toSet
      availableBodies += Body(Seq(Eq(Var(Name("a")), input)))
      cachedCollRelation(collName) = Relation(
        auxCollRel.name,
        auxCollRel.params,
//        auxCollRel.bodies :+ Body(Seq(Eq(Var(Name("a")), input)))
        availableBodies.toSeq
      )
    } else {
      val keysParam: Seq[Param] =
        for ((ty, i) <- mt.keys.zipWithIndex)
          yield Param(Name("k_" + i), demand.TDemand(ty))
      val collRel = Relation(
        collName,
        Param(Name("m"), demand.TDemand(mt)) +:
          keysParam :+ Param(Name("a"), mt.input),
        Seq(Body(Seq(Eq(Var(Name("a")), input))))
      )
      cachedCollRelation += collName -> collRel
    }
    val collAtom: Atom = Call(collName, mono +: keys :+ input)
    Seq(collAtom)


  // Generate the corresponding Coll relation and Agg relation regarding
  // the given MkMono definition.
  private def lowerMkMono(term:MkMono) : Seq[Term] =
    val (in, out): (Type, Type) = term.mono.monotypecheck(term.args) match
      case Left(msg) =>
        (TAny, TAny)
      case Right(tm) => tm
    val mt: TMono = TMono(in, out, term.keys)
    val collName: Name = genCollName(mt)
    if (!cachedCollRelation.contains(collName)) {
      val keys: Seq[Param] =
        for ((ty, i) <- term.keys.zipWithIndex)
          yield Param(Name("k_" + i), demand.TDemand(ty))
      val collRel = Relation(
        collName,
        Param(Name("m"), demand.TDemand(mt)) +:
          keys :+ Param(Name("a"), in),
        Seq()
      )
      cachedCollRelation += collName -> collRel
      val aggName = genAggName(mt)
      val wildCardArgs = mt.keys.map(_ => WildCard)
      val aggArgs: Seq[AggregateArg] = Arg(Var(Name("m"))) +: wildCardArgs :+
        AggregateColumn(Var(Name("b")))
      // TODO: an aggregate operator should be able to receive initializing arguments
      val aggAtom: Aggregate = Aggregate(collName, aggArgs, term.mono)
      aggAtom.addHint(IgnoreCall)
      val aggRel: Relation = Relation(
        aggName,
        Seq(Param(Name("m"), demand.TDemand(mt)), Param(Name("b"), out)),
        Seq(Body(Seq(
          aggAtom
        )))
      )
      cachedAggRelation += aggName -> aggRel
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
    case ResultMono(m) => lowerResultMono(ResultMono(m))
    case _ => super.visitTerm(term)
  }



  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom){atom match
    case AddMono(m, input, keys) => lowerAddMono(AddMono(m, input, keys))
    case _ => super.visitAtom(atom)
  }
