package inca.ir.extension.monotypes

import inca.ir.lowering.BaseLowering
import inca.ir.{Arg, Atom, BaseIR, Body, Call, Eq, ExtensionalCall, Module, ModuleEntry, Name, Param, Relation, TAny, Term, TermArg, Type, Var, WildcardArg, typing}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.demand
import inca.ir.Hint.preserveHints
import inca.ir.typing.IRTypechecker
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.block
import inca.ir.extension.demand.Hints.IgnoreCall
import inca.ir.extension.impure
import inca.ir.extension.impure.Impure
import inca.ir.extension.data
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}
import inca.ir.extension.arithmetic.{Add, DoubleNum, IntNum, TDouble, TInt}
import inca.ir.extension.arithmetic.ArithmeticAggregationOperator.Max as MaxAgg
import inca.ir.extension.bool.{BoolFalse, BoolTrue, TBoolean}
import inca.ir.extension.string.{StringLit, TString}

import scala.collection.mutable


trait Lowering extends BaseLowering:

  private val debug: Boolean = false

  override def loweredIRs: Set[BaseIR] = Set(IR)

  override def requiredIRs: Set[BaseIR] = Set()

  private val cachedAddMonoCtx: mutable.Map[AddMono, AddMonoInfo] = mutable.Map()

  private val cachedResultMonoCtx: mutable.Map[ResultMono, TMono] = mutable.Map()

  private val cachedMkMonoCtx: mutable.Map[MkMono, Seq[Type]] = mutable.Map()

  private val cachedRelation: mutable.Map[Name, Relation] = mutable.Map()

  private val cachedAggRelation: mutable.Map[Name, mutable.Map[Name, Relation]] = mutable.Map()

  private val monoData: DataDefinition = DataDefinition(
    Name("Mono"), Seq(CaseDefinition(Name("mkMono"), Seq(TInt, TString)))
  )

  private var hasMono: Boolean = false

  private var hasMkMono: Boolean = false

  override def visitModule(module: Module): Module =
    val typechecker = new IRTypechecker {}
    typechecker.typecheck(module)
    cachedAddMonoCtx ++= typechecker.getAddMonoInfo
    cachedResultMonoCtx ++= typechecker.getResultMonoInfo
    val m1: Module = super.visitModule(module)
    val m2: Module = Module(
      m1.name,
      m1.lang,
      (if hasMono then Seq(monoData) else Seq()) ++
        m1.contents ++ cachedRelation.map((k, v) => v) ++ mergeCachedAgg()
    )
    m2
  
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
      case AddMono(m, input, keys) =>
        hasMono = true; lowerAddMono(AddMono(m, input, keys))
      case Eq(lhs, rhs, false) => (lhs, rhs) match
        case (Var(v), MkMono(mono, args, keys)) =>
          hasMono = true; lowerMkMono(Var(v), MkMono(mono, args, keys))
        case _ => super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case ResultMono(m) =>
        hasMono = true; lowerResultMono(ResultMono(m))
      case _ => super.visitTerm(term)
  }

  private def genCollName(mt: TMono, neg:Boolean = false): Name =
    val prefix: String = s"Coll$$${mt.input}$$${mt.output}$$" + mt.keys.mkString("$")
    if neg then Name(prefix + "$false") else Name(prefix)

  private def genCollParams(mt: TMono): Seq[Param] =
    val keysParam: Seq[Param] =
      for ((ty, i) <- mt.keys.zipWithIndex)
        yield Param(Name("k_" + i), TDemand(ty))
    Param(Name("m"), TDemand(TData(Name("Mono")))) +:
      keysParam :+
      Param(Name("p"), TBoolean) :+
      Param(Name("a"), TDemand(mt.input))


  private def genCollRel(mt: TMono): Relation =
    Relation(
      genCollName(mt),
      genCollParams(mt),
      Seq(Body(Seq(Eq(Var(Name("p")), BoolTrue))))
    ).addHint(impure.Hints.Pure)

  private def genDefaultValue(ty: Type): Term = ty match
    case TInt => IntNum(0)
    case TString => StringLit("")
    case TDouble => DoubleNum(0)
    case _ => ???

  private def genAuxCollRel(mt: TMono): Relation =
    Relation(
      genCollName(mt, neg = true),
      Seq(
        Param(Name("p"), TBoolean),
        Param(Name("a"), mt.input)
      ),
      Seq(
        Body(Seq(
          Eq(IntNum(1), IntNum(0)),
          Eq(Var(Name("p")), BoolFalse),
          Eq(Var(Name("a")), genDefaultValue(mt.input))
        ))
      )
    ).addHint(impure.Hints.Pure)

  private def genAggName(mt: TMono): Name =
    Name(s"Agg$$${mt.input}$$${mt.output}")

  private def vars(s: String): Seq[Var] =
    s.split(" ").map(v => Var(Name(v)))


  private def genAggRel(mt: TMono, op: MonoDef): Relation =
    val destMono: Atom = Deconstruct(
      Var(Name("m")), Name("mkMono"),
      Seq(Var(Name("id")).arg, Var(Name("name")).arg)
    )
    val opCons: Atom = Eq(Var(Name("name")), StringLit(op.toString))
    val commonAggBody: Seq[Atom] = Seq(destMono, opCons)
    val Seq(p, b): Seq[Term] = vars("p b")
    val agg1Args: Seq[Arg] = TermArg(Var(Name("m"))) +: mt.keys.map(_ => WildcardArg()) :+ TermArg(p) :+ AggregateColumnArg(b)
    val agg1: Aggregate = Aggregate(genCollName(mt), agg1Args, op).addHint(IgnoreCall)
    val body1: Body = Body(commonAggBody :+ Eq(p, BoolTrue) :+ agg1)
    val agg2Args: Seq[Arg] = Seq(TermArg(p), AggregateColumnArg(b))
    val agg2: Aggregate = Aggregate(genCollName(mt, neg = true), agg2Args, op).addHint(IgnoreCall)
    val body2: Body = Body(commonAggBody :+ Eq(p, BoolFalse) :+ agg2)
    val name: Name = genAggName(mt)
    val params: Seq[Param] = Seq(
      Param(Name("m"), TDemand(TData(Name("Mono")))),
      Param(Name("b"), mt.output)
    )
    Relation(name, params, Seq(body1, body2)).addHint(impure.Hints.Pure)

  private def genAggMaxName(mt: TMono): Name =
    Name(s"Agg$$${mt.input}$$${mt.output}$$Max")

  private def genAggRelMax(mt: TMono): Relation =
    val name: Name = genAggMaxName(mt)
    val params: Seq[Param] = Seq(
      Param(Name("m"), TDemand(TData(Name("Mono")))),
      Param(Name("b"), mt.output)
    )
    val body: Body = Body(Seq(
      Aggregate(
        genAggName(mt),
        Seq(TermArg(Var(Name("m"))), AggregateColumnArg(Var(Name("b")))),
        MaxAgg
      )
    ))

    Relation(name, params, Seq(body)).addHint(impure.Hints.Pure)

  private def genImp(mt: TMono, lhs: Var, op: MonoDef): Seq[Atom] =
    val state = Var(Name(gensym.fresh("st")))
    val monoDefId = op.toString
    val monoADT: Construct = Construct(Name("mkMono"), Seq(state, StringLit(monoDefId)))
    val imp: Impure = Impure(
      state,
      Seq(Eq(lhs, monoADT)),
      Add(state, IntNum(1)),
      MonoImpurityKind
    )
    if (!hasMkMono) {
      hasMkMono = true
      val extcall: ExtensionalCall = ExtensionalCall(Name("main$input"), Seq(state.arg))
      Seq(extcall, imp)
    } else {
      Seq(imp)
    }


  private def updateCachedAgg(aggRel: Relation, opName: Name) : Unit =
    val aggName: Name = aggRel.name
    if (!cachedAggRelation.contains(aggName)) {
      val map: mutable.Map[Name, Relation] = mutable.Map()
      map += opName -> aggRel
      cachedAggRelation += aggName -> map
    } else {
      val map: mutable.Map[Name, Relation] = cachedAggRelation(aggName)
      if (!map.contains(opName)) {
        map += opName -> aggRel
      }
    }

  private def mergeCachedAgg(): Seq[Relation] =
    def mergeRelation(rel1: Relation, rel2: Relation) : Relation =
      require(rel1.name == rel2.name)
      require(rel1.params == rel2.params)
      Relation(rel1.name, rel1.params, rel1.bodies ++ rel2.bodies)
    val relations = cachedAggRelation map {
      case (k1, m1) =>
        require(m1.nonEmpty)
        val (nm, rel) = m1.head
        val rels = m1 map { case (k2, v) => v }
        val bodies = rels.foldLeft[Relation](rel)(mergeRelation).bodies
        Relation(
          rel.name,
          rel.params,
          bodies
        ).addHint(impure.Hints.Pure)
    }
    relations.toSeq

  protected def lowerAddMono(atom: AddMono) : Seq[Atom] =
    val mono: Term = atom.m
    val input: Term = atom.input
    val keys: Seq[Term] = atom.keys
    require(cachedAddMonoCtx.contains(atom))
    val info: AddMonoInfo = cachedAddMonoCtx(atom)
    val mt = TMono(info.monoTy.asInstanceOf[TMono].input, info.monoTy.asInstanceOf[TMono].output, info.keysTy)
    val collName: Name = genCollName(mt)
    if (!cachedRelation.contains(collName)) {
      val collRel = genCollRel(mt)
      cachedRelation += collName -> collRel
    }
    val collAtom: Atom = Call(collName, mono.arg +: keys.map(_.arg) :+ BoolTrue.arg :+ input.arg)
    Seq(collAtom)

  private def lowerMkMono(v: Var, term:MkMono) : Seq[Atom] =
    val (in, out): (Type, Type) = term.mono.monotypecheck(term.args) match
      case Left(msg) =>
        (TAny, TAny)
      case Right(tm) => tm
    val mt: TMono = TMono(in, out, term.keys)
    val collName: Name = genCollName(mt)
    val collNameAux: Name = genCollName(mt, neg=true)
    if (!cachedRelation.contains(collName)) {
      cachedRelation += collName -> genCollRel(mt)
      require(!cachedRelation.contains(collNameAux))
      cachedRelation += collNameAux -> genAuxCollRel(mt)
    }
    val aggRel = genAggRel(mt, term.mono)
    updateCachedAgg(aggRel, Name(term.mono.toString))
    val aggMaxName = genAggMaxName(mt)
    if (!cachedRelation.contains(aggMaxName)) {
      cachedRelation += aggMaxName -> genAggRelMax(mt)
    }
    genImp(mt, v, term.mono)


  private def lowerResultMono(term: ResultMono): Seq[Term] =
    require(cachedResultMonoCtx.contains(term))
    val ty: TMono = cachedResultMonoCtx(term)
    val aggName: Name = genAggMaxName(ty)
    val mvar: Var = Var(term.m.asInstanceOf[Var].name)
    val b: Term = Var(Name(mvar.toString + "r"))
    val call: Call = Call(aggName, Seq(mvar.arg, b.arg))
    Seq(block.Block(Seq(call), b))
