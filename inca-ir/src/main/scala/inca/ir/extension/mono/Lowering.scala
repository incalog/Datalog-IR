package inca.ir.extension.mono

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.block.Block
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Name, Param, RefByName, Relation, Term, Type, Var, WildcardArg}
import inca.ir.extension.aggregate
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, DataModuleEntry, Deconstruct, TData, IR as dataIR}
import inca.ir.extension.demand.{DemandIgnoreCallHint, TDemand, IR as demandIR}
import inca.ir.extension.impure.{Impure, IR as impureIR}
import inca.ir.extension.map.{MapComprehension, MapFun}
import inca.ir.extension.set.SetComprehension
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.extension.tuple.{Project, TTuple}
import inca.ir.lowering.BaseLowering

trait Lowering(optimizeMono: Boolean = true) extends BaseLowering:
  override val name: String = s"Mono(optimize = $optimizeMono)"
  override def loweredIRs: Set[BaseIR] = Set(IR)
  override def requiredIRs: Set[BaseIR] = Set(aggregate.IR, demandIR, impureIR, dataIR)

  private def normName(s: String) : String =
    Seq("(", ")", "[", "]", ", ").foldLeft(s)((s, t) => s.replace(t, "$")).replaceAll("\\${2,}", "\\$")


  /** Each mono kind gets its own data type, based on input, output, and keys */
  def monoDataType(tm: TMono): TData =
    TData(Name(normName(s"Mono_${tm.input}_${tm.output}$$${tm.keys.mkString("_")}")))

  def monoCollectName(tm: TMono): Name = Name(normName("Collect_" + monoDataType(tm).ref.name))
  def monoAggregateName(tm: TMono): Name = Name(normName("Aggregate_" + monoDataType(tm).ref.name))

  def monoDataConstructor(mono: MonoDefinition, keys: Seq[Type]): Name =
    val tm = mono.monoType(keys)
    Name(normName(s"${mono.name}$$${tm.keys.mkString("_")}"))

  def createDataDefinition(tm: TMono, monos: Seq[MonoDefinition]): Seq[DataModuleEntry] =
    val data = DataDefinition(monoDataType(tm).ref.name)
    val cases = monos.map(mono =>
      CaseDefinition(monoDataConstructor(mono, tm.keys), TInt +: TString +: mono.constructorParamTypes, TData(data.name))
    )
    data +: cases

  private def createCollParams(tm: TMono): Seq[Param] =
    val data = monoDataType(tm)
    val keyParams = tm.keys.zipWithIndex.map((ty, ix) => Param(Name(s"key_$ix"), TDemand(ty)))
    Param(Name("m"), TDemand(data)) +: keyParams :+ Param(Name("input"), TDemand(tm.input))

  def createCollectingRelation(tm: TMono): Relation =
    Relation(monoCollectName(tm), createCollParams(tm), Seq(Body(Seq())))

  def createAggregationRelation(tm: TMono, monos: Seq[MonoDefinition]): Relation =
    val params = Seq(Param(Name("m"), TDemand(monoDataType(tm))), Param(Name("output"), tm.output))
    val bodies = monos.map { mono =>
      val constr = monoDataConstructor(mono, tm.keys)
      val args = Var(Name("id")) +: Var(Name("name")) +: mono.constructorParamTypes.zipWithIndex.map((_,ix) => Var(Name(s"arg_$ix")))
      val destruct = Deconstruct(Var(Name("m")), RefByName(constr), args.map(_.arg), false)

      val atoms = mono match
        case _: SetMonoDefinition if optimizeMono =>
          optimizeSetMono(tm)
//        case mm@MapMonoDefinition(keyTy1, MapMonoDefinition(_, _)) => optimizeNestedMapMono(tm, mm)
        case m: MapMonoDefinition if optimizeMono => optimizeMapMono(tm, m)
        case _ =>
          val keyArgs = tm.keys.map(_ => WildcardArg())
          val aggArgs = Var(Name("m")).arg +: keyArgs :+ AggregateColumnArg(Var(Name("state")))

          val op = MonoAggregationOperator(mono)
          val aggregate = Aggregate(RefByName(monoCollectName(tm)), aggArgs, op).addHint(DemandIgnoreCallHint)
          val project = Eq(Var(Name("output")), mono.resultTerm(Var(Name("state")), gensym))
          Seq(aggregate, project)
      Body(destruct +: atoms)
    }
    Relation(monoAggregateName(tm), params, bodies)


  private def optimizeSetMono(tm: TMono): Seq[Atom] =
    val keyArgs = tm.keys.map(_ => WildcardArg())
    val collArgs = Var(Name("m")).arg +: keyArgs :+ Var(Name("elem")).arg
    val project = Eq(Var(Name("output")),
      SetComprehension(
        Var(Name("elem")),
        Seq(Call(RefByName(monoCollectName(tm)), collArgs, false).addHint(DemandIgnoreCallHint))
      )
    )
    Seq(project)

  /**
   * Map Mono Optimization sketch:
   *
   * Coll(mm: MapMono, kv: TDemand(TTuple(K,V))) = nil
   * Agg(mm: MapMono, m: TMap(K,V)) = aggregate(Call(Coll(mm, #m)), mm.monoOp)
   * ~>
   * Coll(mm: MapMono, kv: TDemand(TTuple(K,V))) = nil
   * Coll$split(mm: MapMono, k: TDemand(K), v: TDemand(V)) = Coll(mm, kv), k == kv._1, v == kv._2
   * Agg(mm: MapMono, m: TMap(K,V)) =
   * m == {(k,v) |
   * Coll$split(mm, k, _),
   * aggregate(Call(Coll$split(mm, k, #v)), mm.valMono.monoOp)
   * }
   *
   * // for nested map monos
   * Agg(mm: MapMono, m: TMap(K,V)) =
   * m == {(k,v) |
   * Coll$split(mm, k, _),
   * v == {(k2,v2) |
   * Coll$split$split(mm, k, k2, _),
   * aggregate(Call(Coll$split(mm, k, k2, #v2)), mm.valMono.monoOp)
   * }
   * }
   *
   */
  private def optimizeMapMono(tm: TMono, m: MapMonoDefinition): Seq[Atom] =
    var valueMono: Option[MonoDefinition] = None

    def collInputTyp(mono: MapMonoDefinition): Seq[Type] = mono match
      case MapMonoDefinition(ty1, mm@MapMonoDefinition(ty2, valMono)) => ty1 +: collInputTyp(mm)
      case MapMonoDefinition(ty, mm) => valueMono = Some(mm); Seq(ty, mm.typ.in)

    val inputTys = collInputTyp(m)
    val keyTys = inputTys.dropRight(1)
    val valTy = inputTys.last
    val collName = monoCollectName(tm).name
    val collParams = createCollParams(tm).map(p => Param(p.name, p.ty match
      case TDemand(ty) => ty
      case ty => ty
    ))
    val splitCollName = Name(gensym.fresh(collName + "$split"))
    val keyParams = keyTys.map(ty => Param(gensym.freshName(Name("key")), ty))
    val vParam = Param(gensym.freshName(Name("value")), valTy)
    val args = collParams.map(p => Var(p.name).arg)

    def projNestedTuple(tp : Term, params: Seq[Param]): Seq[Eq] = params match
      case Seq(param) => Seq(Eq(tp, Var(param.name)))
      case param +: tail => Eq(Project(tp, 0), Var(param.name)) +: projNestedTuple(Project(tp, 1), tail)


    val optCollRel = Relation(
      Name(gensym.fresh(collName + "$split")),
      collParams.dropRight(1) ++ keyParams :+ vParam,
      Seq(Body(
        Call(Name(collName), args).addHint(DemandIgnoreCallHint) +:
        projNestedTuple(Var(collParams.last.name), keyParams :+ vParam),
      ))
    )
    mapMonoColl += optCollRel
    val keyNum = keyParams.size
    val callAtom = Call(
        optCollRel.name,
        Var(Name("m")).arg +: (tm.keys.map(_ => WildcardArg()) ++
          (0 until keyNum).map(i => Var(Name(s"k$i")).arg)) :+ WildcardArg()
    ).addHint(DemandIgnoreCallHint)


    def createMapFun(i: Int): MapFun =
      val param = Param(Name(s"k$i"), keyTys(i))
      val tm = if i == keyNum - 1 then
        valueMono.getOrElse(throw IllegalStateException(s"value mono is not expected to be None")) match
          case MapMonoDefinition(_, _) => throw IllegalStateException(s"$valueMono is not expected to be a map mono")
          case SetMonoDefinition(_) =>
            val elem = gensym.fresh("elem")
            SetComprehension(
              Var(Name(elem)),
              Seq(
                callAtom.copy(args = callAtom.args.dropRight(1) :+ Var(Name(elem)).arg)
              )
            )
          case vmono =>
            Block(Seq(
              callAtom, Aggregate(
                RefByName(optCollRel.name),
                callAtom.args.dropRight(1) :+ AggregateColumnArg(Var(Name("v"))),
                MonoAggregationOperator(valueMono.get)
              )), Var(Name("v")))
      else createMapFun(i + 1)
      MapFun(Seq(param), tm)

    val project = m.resultTerm(createMapFun(0), gensym)
    Seq(Eq(project, Var(Name("output"))))



  var monoDefs: Set[(MonoDefinition, Seq[Type])] = _
  var monoTypes: Set[TMono] = _
  var mapMonoColl: Set[Relation] = _

  override def visitModule(module: ir.Module): ir.Module =
    monoDefs = Set()
    monoTypes = Set()
    mapMonoColl = Set()
    val mod = super.visitModule(module)

    val defaultTypes = monoTypes.map(t => t -> Set()).toMap
    val defsByType = defaultTypes ++ monoDefs.groupBy((mono, keys) => mono.monoType(keys))
    val dataDefs = defsByType.flatMap((tm, defs) => createDataDefinition(tm, defs.toSeq.map(_._1))).toSeq

    val collectRels = monoTypes.toSeq.map(createCollectingRelation)
    val aggregateRels = defsByType.map((tm, defs) => createAggregationRelation(tm, defs.toSeq.map(_._1))).toSeq

//    val monoResultRels = monoDefs.toSeq.map((mono, _) => mono.resultRelation)

    mod.copy(contents = dataDefs ++ mod.contents ++ collectRels ++ aggregateRels ++ mapMonoColl)

  override def visitType(ty: Type): Type = ty match
    case tm@TMono(in, out, keys) =>
      monoTypes += tm
      monoDataType(tm)
    case _ => super.visitType(ty)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case NewMono(mono, keys, args) =>
      monoDefs += (mono, keys)
      monoTypes += mono.monoType(keys)
      val dataConstr = monoDataConstructor(mono, keys)
      val stVar = Name(gensym.fresh("monoCount"))
      val mVar = Var(Name(gensym.fresh("mono")))
      val constr = Construct(RefByName(dataConstr), Var(stVar) +: StringLit(mono.name.name) +: args)
      val imp = Impure.counter(stVar, Eq(mVar, constr), MonoImpurityKind)
      val block = Block(imp, mVar)
      Seq(block)
    case ReadMono(m) =>
      val tm = m.typ.get.ty.asInstanceOf[TMono]
      val output = Name(gensym.fresh("output"))
      val name = monoAggregateName(tm)
      val call = Call(name, Seq(m.arg, Var(output).arg))
      Seq(Block(call, Var(output)))
    case _ => super.visitTerm(term)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
    case WriteMono(m, input, keys) =>
      val tm = m.typ.get.ty.asInstanceOf[TMono]
      val args = m +: keys :+ input
      Seq(Call(monoCollectName(tm), args.map(_.arg)))
    case _ => super.visitAtom(atom)
  }
