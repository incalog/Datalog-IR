package inca.ir.extension.mono

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.block.Block
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Name, Param, RefByName, Relation, Term, Type, Var, WildcardArg}
import inca.ir.extension.data.{Deconstruct, Construct, TData, DataDefinition, CaseDefinition, DataModuleEntry, IR as dataIR}
import inca.ir.extension.demand.{DemandIgnoreCallHint, TDemand, IR as demandIR}
import inca.ir.extension.impure.{Impure, IR as impureIR}
import inca.ir.extension.string.{StringLit, TString}
import inca.ir.lowering.BaseLowering

trait Lowering extends BaseLowering:
  override val name: String = "Mono"
  override def loweredIRs: Set[BaseIR] = Set(IR)
  override def requiredIRs: Set[BaseIR] = Set(demandIR, impureIR, dataIR)

  private def normName(s: String) : String =
    Seq("(", ")", "[", "]", ", ").foldLeft(s)((s, t) => s.replace(t, "$"))

  /** Each mono kind gets its own data type, based on input, output, and keys */
  def monoDataType(tm: TMono): TData =
    TData(Name(normName(s"Mono_${tm.input}_${tm.output}$$${tm.keys.mkString("_")}")))

  def monoCollectName(tm: TMono): Name = Name(normName("Collect_" + monoDataType(tm).ref.name))
  def monoAggregateName(tm: TMono): Name = Name(normName("Aggregate_" + monoDataType(tm).ref.name))

  def monoDataConstructor(mono: MonoDefinition, keys: Seq[Type]): Name =
    val tm = mono.monoType(keys)
    Name(normName(s"Mono_${tm.input}_${tm.output}$$${tm.keys.mkString("_")}_${mono.name}"))

  def createDataDefinition(tm: TMono, monos: Seq[MonoDefinition]): Seq[DataModuleEntry] =
    val data = DataDefinition(monoDataType(tm).ref.name)
    val cases = monos.map(mono =>
      CaseDefinition(monoDataConstructor(mono, tm.keys), TInt +: TString +: mono.constructorParamTypes, TData(data.name))
    )
    data +: cases

  def createCollectingRelation(tm: TMono): Relation =
    val data = monoDataType(tm)
    val keyParams = tm.keys.zipWithIndex.map((ty,ix) => Param(Name(s"key_$ix"), TDemand(ty)))
    val params = Param(Name("m"), TDemand(data)) +: keyParams :+ Param(Name("input"), TDemand(tm.input))
    Relation(monoCollectName(tm), params, Seq(Body(Seq())))

  def createAggregationRelation(tm: TMono, monos: Seq[MonoDefinition]): Relation =
    val params = Seq(Param(Name("m"), TDemand(monoDataType(tm))), Param(Name("output"), tm.output))
    val bodies = monos.map { mono =>
      val constr = monoDataConstructor(mono, tm.keys)
      val args = Var(Name("id")) +: Var(Name("name")) +: mono.constructorParamTypes.zipWithIndex.map((_,ix) => Var(Name(s"arg_$ix")))
      val destruct = Deconstruct(Var(Name("m")), RefByName(constr), args.map(_.arg), false)

      val keyArgs = tm.keys.map(_ => WildcardArg())
      val aggArgs = Var(Name("m")).arg +: keyArgs :+ AggregateColumnArg(Var(Name("state")))

      val op = MonoAggregationOperator(mono)
      val aggregate = Aggregate(RefByName(monoCollectName(tm)), aggArgs, op).addHint(DemandIgnoreCallHint)
      val project = Eq(Var(Name("output")), mono.resultTerm(Var(Name("state"))))
      Body(Seq(destruct, aggregate, project))
    }
    Relation(monoAggregateName(tm), params, bodies)

  var monoDefs: Set[(MonoDefinition, Seq[Type])] = _
  var monoTypes: Set[TMono] = _

  override def visitModule(module: ir.Module): ir.Module =
    monoDefs = Set()
    monoTypes = Set()
    val mod = super.visitModule(module)

    val defaultTypes = monoTypes.map(t => t -> Set()).toMap
    val defsByType = defaultTypes ++ monoDefs.groupBy((mono, keys) => mono.monoType(keys))
    val dataDefs = defsByType.flatMap((tm, defs) => createDataDefinition(tm, defs.toSeq.map(_._1))).toSeq

    val collectRels = monoTypes.toSeq.map(createCollectingRelation)
    val aggregateRels = defsByType.map((tm, defs) => createAggregationRelation(tm, defs.toSeq.map(_._1))).toSeq

//    val monoResultRels = monoDefs.toSeq.map((mono, _) => mono.resultRelation)
    
    mod.copy(contents = dataDefs ++ mod.contents ++ collectRels ++ aggregateRels)

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
