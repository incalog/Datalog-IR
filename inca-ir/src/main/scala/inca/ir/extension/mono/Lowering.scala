package inca.ir.extension.mono

import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Call, Eq, ExtensionalCall, Module, ModuleEntry, Name, Neq, Param, Relation, TAny, Term, Type, Var, typing}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.demand
import inca.ir.Hint.preserveHints
import inca.ir.typing.IRTypechecker
import inca.ir.extension.aggregate
import inca.ir.extension.aggregate.AggregateArg.{AggregateColumn, Arg, WildCard}
import inca.ir.extension.aggregate.{Aggregate, AggregateArg, AggregationOperatorUserDefined}
import inca.ir.extension.block.Block
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

/** Lowering of Mono-type extension.

    Current limitations:
    1. All MkMonos are assumed to be in the main rule, as they share the same impure state.
 */
trait Lowering extends BaseLowering:

  private val debug: Boolean = false

  override def loweredIRs: Set[BaseIR] = Set(IR)

  override def requiredIRs: Set[BaseIR] = Set()

  // Mapping from an AddMono atom to the type information of mono-type variable, input term and keys
  // (used to find the corresponding Coll relation of an AddMono)
  private val cachedAddMonoCtx: mutable.Map[WriteMono, AddMonoInfo] = mutable.Map()

  // Mapping from a ResultMono Term to the type of its inside mono-type variables
  // (used to find the corresponding aggregation relation of a ResultMono)
  private val cachedResultMonoCtx: mutable.Map[ReadMono, TMono] = mutable.Map()

  // Record the newly generated relations (except relations used to do mono aggregation) during lowering
  private val cachedRelation: mutable.Map[Name, Relation] = mutable.Map()

  // Since two Mono definitions having the same input and output type signature share the same aggregation
  // relation name (they will be distinguished by their names), it is cumbersome to update the generated aggregation relation
  // directly. We use cachedAggRelation to specify the body of an mono aggregation relation by the name of aggregation relation
  // and the name of mono definition.
  private val cachedAggRelation: mutable.Map[Name, mutable.Map[Name, Relation]] = mutable.Map()

  // The ADT definition of mono-type instances, the meaning of type signature:
  // - TInt: record the OID
  // - TString: record the name of mono definition
  // - (TODO) Seq[Type]: the type of arguments to instantiating a mono-type instance
  private val monoData: DataDefinition = DataDefinition(
    Name("Mono"), Seq(CaseDefinition(Name("mkMono"), Seq(TInt, TString)))
  )

  // Used to track if there exists Mono-type IR atom in the program (if
  // there is no Mono-type IR atom or term in the program,
  // there is no need to introduce the monoData ADT.
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
      case WriteMono(m, input, keys) => hasMono = true; lowerAddMono(WriteMono(m, input, keys))
      case _ => super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) {
    term match
      case NewMono(mono, keys, args) => hasMono = true; lowerMkMono(NewMono(mono, keys, args))
      case ReadMono(m) => hasMono = true; lowerResultMono(ReadMono(m))
      case _ => super.visitTerm(term)
  }

  /** Generate a collection relation name via the type of mono and a boolean variable.
   *  Since sometimes the Coll relation for a mono is empty
   *  (if there does not exist a corresponding AddMono), in which case the Coll rule will be
   *  eliminated when generating VIATRA code (GeneratePsystem.scala). To resolve it, we generate
   *  two collection relation for each TMono and use `neg` as a sign.
   */
  private def genCollName(mt: TMono, neg:Boolean = false): Name =
    val prefix: String = s"Coll$$${mt.input}$$${mt.output}$$" + mt.keys.mkString("$")
    if neg then Name(prefix + "$false") else Name(prefix)


  /** Generate the parameters of a collection relation. The parameters can be divided into four parts:
   * 1. Mono-type parameter: it should be a demand parameter.
   * 2. Keys: used to uniquely identify the input to mono at different locations in the program, which also should be
   *    demand parameters because we want to do the bindings of each key in the body via demand transformation.
   * 3. A boolean parameter to denote whether it is used for doing aggregation on empty body or not.
   * 4. Input to the mono.
   */
  private def genCollParams(mt: TMono): Seq[Param] =
    val keysParam: Seq[Param] =
      for ((ty, i) <- mt.keys.zipWithIndex)
        yield Param(Name("k_" + i), TDemand(ty))
    Param(Name("m"), TDemand(TData(Name("Mono")))) +:
      keysParam :+
      Param(Name("p"), TBoolean) :+
      Param(Name("a"), TDemand(mt.input))


  /**
   * Generate the collection relation, as there will not be MkMono in the rules except main,
   * we add a pure key to it to reduce complexity of programs after lowering (might be changed
   * in the future if we want to allocate impure objects in other rules).
   */
  private def genCollRel(mt: TMono): Relation =
    Relation(
      genCollName(mt),
      genCollParams(mt),
      Seq(Body(Seq(Eq(Var(Name("p")), BoolTrue))))
    ).addHint(impure.Hints.Pure)

  /**
   * Mapping from each type to its initial value. This method will also be removed in the future
   * if we can do aggregation on empty relation.
   */
  private def genDefaultValue(ty: Type): Term = ty match
    case TInt => IntNum(0)
    case TString => StringLit("")
    case TDouble => DoubleNum(0)
    case _ => ???

  /**
   * Generate the auxiliary collection relation which serve as the alternative for doing aggregation
   * if the original collection relation's body is empty.
   */
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
    s.split(" ").toSeq.map(v => Var(Name(v)))

  /**
   * Generate an aggregation relation. We use the name of Mono definition to distinguish monos having
   * the same input and output type. For each mono definition, we generate two bodies to deal with
   * aggregation on empty and non-empty multisets respectively.
   */
  private def genAggRel(mt: TMono, op: MonoDefinition): Relation =
    val destMono: Atom = Deconstruct(
      Var(Name("m")), Name("mkMono"),
      Seq(Var(Name("id")), Var(Name("name")))
    )
    val opCons: Atom = Eq(Var(Name("name")), StringLit(op.toString))
    val commonAggBody: Seq[Atom] = Seq(destMono, opCons)
    val Seq(p, b): Seq[Term] = vars("p b")
    val agg1Args: Seq[AggregateArg] = Arg(Var(Name("m"))) +:
        mt.keys.map(_ => WildCard(Var(Name(gensym.fresh("k")))))
        :+ Arg(p) :+ AggregateColumn(b)
    val agg1: Aggregate = Aggregate(genCollName(mt), agg1Args, new MonoAggregationOperator(op)).addHint(IgnoreCall)
    val body1: Body = Body(commonAggBody :+ Eq(p, BoolTrue) :+ agg1)
    val agg2Args: Seq[AggregateArg] = Seq(Arg(p), AggregateColumn(b))
    val agg2: Aggregate = Aggregate(genCollName(mt, neg = true), agg2Args, new MonoAggregationOperator(op)).addHint(IgnoreCall)
    val body2: Body = Body(commonAggBody :+ Eq(p, BoolFalse) :+ agg2)
    val name: Name = genAggName(mt)
    val params: Seq[Param] = Seq(
      Param(Name("m"), TDemand(TData(Name("Mono")))),
      Param(Name("b"), mt.output)
    )
    Relation(name, params, Seq(body1, body2)).addHint(impure.Hints.Pure)

  private def genAggMaxName(mt: TMono): Name =
    Name(s"Agg$$${mt.input}$$${mt.output}$$Max")

  /**
   * As there are two bodies for each mono definition, if the multiset for aggregation is non-empty,
   * we can get two results from the aggregation relation. But as the set of aggregation result has a partial order,
   * we can define a relation to find the larger aggregation result.
   */
  private def genAggRelMax(mt: TMono): Relation =
    val name: Name = genAggMaxName(mt)
    val params: Seq[Param] = Seq(
      Param(Name("m"), TDemand(TData(Name("Mono")))),
      Param(Name("b"), mt.output)
    )
    val body: Body = Body(Seq(
      Aggregate(
        genAggName(mt),
        Seq(Arg(Var(Name("m"))), AggregateColumn(Var(Name("b")))),
        MaxAgg
      )
    ))

    Relation(name, params, Seq(body)).addHint(impure.Hints.Pure)

  private def genImp(mt: TMono, op: MonoDefinition): Seq[Term] =
    val state = Var(Name(gensym.fresh("st")))
    val monoDefId = op.toString
    val monoADT: Construct = Construct(Name("mkMono"), Seq(state, StringLit(monoDefId)))
    val freshMono: Var = Var(Name(gensym.fresh("m")))
    val imp: Impure = Impure(
      state,
      Seq(Eq(freshMono, monoADT)),
      Add(state, IntNum(1)),
      MonoImpurityKind
    )
    if (!hasMkMono) {
      hasMkMono = true
      val extcall: ExtensionalCall = ExtensionalCall(Name("main$input"), Seq(state))
      Seq(Block(Seq(extcall, imp), freshMono))
    } else {
      Seq(Block(Seq(imp), freshMono))
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

  /**
   * An AddMono will be lowered into a collection relation. If the collection relation does not exist,
   * we can create it according to the type information retrieved during type checking.
   */
  protected def lowerAddMono(atom: WriteMono) : Seq[Atom] =
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
    val collAtom: Atom = Call(collName, mono +: keys :+ BoolTrue :+ input)
    Seq(collAtom)

  /**
   * Lower MkMono into an ADT instance and generate corresponding collection and aggregation relations.
   */
  private def lowerMkMono(term:NewMono) : Seq[Term] =
    val (in, out) = term.mono.typecheckConstructor(term.args.map(_.typ.get.ty)) match
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
    genImp(mt, term.mono)


  /**
   * Lower ResultMono into finding the maximum result of aggregation on the corresponding mono-type variables
   * (as there are two bodies for each mono definition).
   */
  private def lowerResultMono(term: ReadMono): Seq[Term] =
    require(cachedResultMonoCtx.contains(term))
    val ty: TMono = cachedResultMonoCtx(term)
    val aggName: Name = genAggMaxName(ty)
    val mvar: Var = Var(term.m.asInstanceOf[Var].name)
    val b: Term = Var(Name(mvar.toString + "r"))
    val call: Call = Call(aggName, Seq(mvar, b))
    Seq(Block(Seq(call), b))

case class MonoAggregationOperator(mono: MonoDefinition) extends AggregationOperatorUserDefined:
  override def typecheck(in: Seq[Type]): Either[String, Type] = ???

