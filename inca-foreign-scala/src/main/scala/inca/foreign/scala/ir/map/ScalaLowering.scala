package inca.foreign.scala.ir.map

import inca.foreign.scala.ir.primitive.ScalaConstantTerm.TRUE
import inca.foreign.scala.ir.primitive.{ScalaAggregationAtom, ScalaConstantTerm, ScalaMonoAggregationOperator, ScalaTerm, ScalaType, ScalaLowering as BaseScalaLowering}
import inca.ir.extension.set.SetFrom
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Module, Name, Param, Relation, Term, Type, Var, WildcardArg, string2name}
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.block.Block
import inca.ir.extension.demand.{DemandIgnoreCallHint, TDemand}
import inca.ir.extension.disjunction.Disjunction
import inca.ir.extension.map.{MapComprehension, MapConcat, MapContains, MapFrom, MapFun, MapLit, MapLookUp, MapPlus, MapUnion, TMap, IR as mapIR}
import inca.ir.extension.{block, demand}
import inca.ir.extension.mono.{MapMonoDefinition, MonoAggregationOperator}
import inca.ir.extension.tuple.{TTuple, TupleLit}

import scala.collection.mutable.ListBuffer


trait ScalaLowering extends BaseScalaLowering:
  override def loweredIRs: Set[BaseIR] = Set(mapIR)
  override def requiredIRs: Set[BaseIR] = super.requiredIRs ++ Set(demand.IR, block.IR, mapIR)

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TMap(_, _) => true
    case _ => super.isTypeSupported(ty)

  inline private def getScalaKVType(map: Term): (String, String) =
    val mapTy = map.typ.get.ty.asInstanceOf[TMap]
    (visitType(mapTy.k).asInstanceOf[ScalaType].name, visitType(mapTy.v).asInstanceOf[ScalaType].name)


  private var mapCompCollRelations: Set[Relation] = _
  private var mapMembershipRelations: Set[Relation] = _
  private var mapCompAggCounter: Int = _
  private var currentModule: Module = _

  override def visitModule(module: Module): Module =
    mapCompAggCounter = 0
    mapCompCollRelations = Set()
    mapMembershipRelations = Set()
    currentModule = module
    val mod = super.visitModule(module)
    mod.copy(contents = mod.contents ++ mapCompCollRelations ++ mapMembershipRelations)

  private def createMapAggOp(keyTy: String, valueTy: String): ScalaMonoAggregationOperator =
    ScalaMonoAggregationOperator(
      name = s"AggOp$$Map$$$keyTy$$$valueTy",
      inputTy = ScalaType(s"($keyTy, $valueTy)"),
      stateTy = ScalaType(s"Map[$keyTy, $valueTy]"),
      initCode = s"Map[$keyTy, $valueTy]()",
      addCode = s"(st: Map[$keyTy, $valueTy], a: ($keyTy, $valueTy)) => st + (a._1 -> a._2)"
    )

  /** Return the Scala Type name of the input term.  */
  inline private def getSType(term: Term): String =
    visitType(term.typ.getOrElse(throw new IllegalArgumentException(s"$term's type is not available")).ty).asInstanceOf[ScalaType].name


  inline private def isMapRelation(relName: Name): Boolean =
    val relation = currentModule.relations.getOrElse(relName.name, throw new IllegalStateException(s"Unknown relation $name"))
    relation.params.size > 1

  inline private def mkScalaTuple(args: Seq[String]): String =
    if args.size == 1 then
      args.head
    else
      args.mkString("(", ", ", ")")

  /** Return the scala type name of input inca type */
  inline private def getSTName(ty: Type): String = visitType(ty).asInstanceOf[ScalaType].name


  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case MapLit(ts) =>
      val (keyTy, valueTy) = getScalaKVType(term)
      val tts: Seq[ScalaTerm] = ts.flatMap((k, v) => visitTerm(k).zip(visitTerm(v)).map((k, v) => ScalaTerm(
        s"(k: $keyTy, v: $valueTy) => (k, v)",
        ScalaType(s"($keyTy, $valueTy)"),
        Seq(k, v)
      )))
      val params = tts.indices map (i => s"p$i: ($keyTy, $valueTy)")
      val args = tts.indices map (i => s"p$i._1 -> p$i._2")
      Seq(ScalaTerm(s"(${params.mkString(", ")}) => Map[$keyTy, $valueTy](${args.mkString(", ")})", ScalaType(s"Map[$keyTy, $valueTy]"), tts))
    case MapLookUp(map, key) =>
      val (keyTy, valueTy) = getScalaKVType(map)
      val Seq((smap, skey)) = visitTerm(map).zip(visitTerm(key))
      Seq(ScalaTerm(s"(map: Map[$keyTy, $valueTy], key: $keyTy) => map(key)", ScalaType(valueTy), Seq(smap, skey)))
    case MapPlus(map, key, value) =>
      val (keyTy, valueTy) = getScalaKVType(map)
      val Seq(((smap, skey), svalue)) = visitTerm(map).zip(visitTerm(key)).zip(visitTerm(value))
      Seq(ScalaTerm(
        s"(map: Map[$keyTy, $valueTy], key: $keyTy, value: $valueTy) => map + (key -> value)",
        ScalaType(s"Map[$keyTy, $valueTy]"),
        Seq(smap, skey, svalue)
      ))
    case MapUnion(map1, map2) =>
      val Seq(st1) = visitTerm(map1)
      val Seq(st2) = visitTerm(map2)
      val unionMap = gensym.freshName("unionMap")
      Seq(Block(Disjunction(Seq(Eq(Var(unionMap), st1)), Seq(Eq(Var(unionMap), st2))), Var(unionMap)))
    case MapConcat(map1, map2) =>
      val Seq(st1) = visitTerm(map1)
      val Seq(st2) = visitTerm(map2)
      val (keyTy, valueTy) = getScalaKVType(map1)
      val mapTy = s"Map[$keyTy, $valueTy]"
      Seq(ScalaTerm(
        s"(map1: $mapTy, map2: $mapTy) => map1 ++ map2",
        ScalaType(mapTy),
        Seq(st1, st2)
      ))
    // Two reasons for not translating MapFun:
    // 1. The input information of MapFun is not available, MapFun might be translated into an infinite map
    //    if the domain of parameters is infinite.
    // 2. The aggregation result map will never interact with MapFun terms by map operations
    //    in the high-level languages.
    case MapFun(params, valTerm) => Seq(MapFun(params, valTerm))
    case MapComprehension(key, value, atoms) =>
      val keyTy = getSTName(key.typ.get.ty)
      val valueTy = getSTName(value.typ.get.ty)
      val vars = term.vars
      vars.foreach(v => v.typ.getOrElse(throw new IllegalStateException(s"Set lowering requires types IR in $v")))
      val (boundVars, bindingVars) = vars.partition(!_.typ.get.mode.isBinding)
      val freeVars = boundVars.toSet diff bindingVars.toSet
      val inputParams = freeVars.toSeq.map(v => v.name -> TDemand(visitType(v.typ.get.ty).asInstanceOf[ScalaType]))
      val ats = atoms.flatMap(visitAtom)
      val Seq(pair) = visitTerm(TupleLit.make(Seq(key, value)))
      val collName = gensym.freshName(s"coll$$map$$comp$$$mapCompAggCounter")
      val pairName = gensym.freshName(s"kvPair")
      mapCompCollRelations += Relation(
        collName,
        inputParams.map((nm, ty) => Param(nm, ty)) :+ Param(pairName, ScalaType(s"($keyTy, $valueTy)")),
        Seq(Body(ats :+ Eq(pair, Var(pairName))))
      )
      mapCompAggCounter += 1
      val aggOp = createMapAggOp(keyTy, valueTy)
      val resName = gensym.freshName("map$comp$res")
      val aggAtom = ScalaAggregationAtom(aggOp, collName, Var(resName), inputParams.map((nm, _) => Var(nm)) :+ Var(gensym.freshName("_")), inputParams.size)
      val callAtom = Call(collName, inputParams.map((nm, _) => Var(nm).arg) :+ WildcardArg())
      Seq(Block(Seq(callAtom, aggAtom), Var(resName)))
    case MapFrom(name) if isMapRelation(name) =>
      val Seq(rel) = visitRelation(currentModule.relations(name.name))
      val (demandedTys, nonDemandedTys) = rel.params.map(_.ty).partition(_.isInstanceOf[TDemand])
      val keyTy = TTuple.make(demandedTys.map(_.asInstanceOf[TDemand].ty))
      val valueTy = TTuple.make(nonDemandedTys)
      val keySTy = getSTName(keyTy)
      val valueSTy = getSTName(valueTy)
      val collName = s"coll$$map$$from$$$name"
      val demandedVars = ListBuffer[Var]()
      val nonDemandedVars = ListBuffer[Var]()
      val args = rel.params.map(_.ty).zipWithIndex.map {
        case (TDemand(_), i) => demandedVars += Var(s"v$i"); Var(s"v$i").arg
        case (_, i) => nonDemandedVars += Var(s"v$i"); Var(s"v$i").arg
      }
      val pairName = "kvPair"
      val pairTy = ScalaType(s"($keySTy, $valueSTy)")
      val keyTerm = ScalaTerm(
        s"(${demandedTys.zipWithIndex.map((ty, i) => s"v$i: ${getSTName(ty.asInstanceOf[TDemand].ty)}").mkString(", ")}) => ${mkScalaTuple(demandedTys.indices.map(i => s"v$i"))}",
        ScalaType(keySTy),
        demandedVars.toSeq
      )
      val valueTerm = ScalaTerm(
        s"(${nonDemandedTys.zipWithIndex.map((ty, i) => s"v$i: ${getSTName(ty)}").mkString(", ")}) => ${mkScalaTuple(nonDemandedTys.indices.map(i => s"v$i"))}",
        ScalaType(valueSTy),
        nonDemandedVars.toSeq
      )
      val pairTerm = ScalaTerm(
        s"(k: $keySTy, v: $valueSTy) => (k, v)",
        pairTy,
        Seq(Var("k"), Var("v"))
      )
      val collRel = Relation(
        collName,
        Seq(Param(pairName, pairTy)),
        Seq(Body(Seq(
          Call(name, args).addHint(DemandIgnoreCallHint),
          Eq(Var("k"), keyTerm),
          Eq(Var("v"), valueTerm),
          Eq(Var(pairName), pairTerm)
        )))
      )
      val aggOp = createMapAggOp(keySTy, valueSTy)
      val resNm = gensym.freshName("map$from$res")
      val aggAtom = ScalaAggregationAtom(aggOp, collName, Var(resNm), Seq(Var(gensym.freshName("_"))), 0)
      mapCompCollRelations += collRel
      Seq(Block(Seq(aggAtom), Var(resNm)))
    case MapFrom(name) if !isMapRelation(name) => visitTerm(SetFrom(name))
    case _ => super.visitTerm(term)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
    case MapContains(map, key) =>
      val (keyTy, valueTy) = getScalaKVType(map)
      val Seq((smap, skey)) = visitTerm(map).zip(visitTerm(key))
      val memRelName = createRelName(s"Map$$Keys$$$keyTy")
      val memRel = Relation(
        memRelName,
        Seq(Param("key", ScalaType(keyTy)), Param("map", TDemand(ScalaType(s"Map[$keyTy, $valueTy]")))), Seq(
        Body(Seq(
          Eq(ScalaTerm(s"(map: Map[$keyTy, $valueTy]) => map.nonEmpty", ScalaType.bool, Seq(Var("map"))), TRUE),
          Eq(ScalaTerm(s"(map: Map[$keyTy, $valueTy]) => map.head._1", ScalaType(keyTy), Seq(Var("map"))), Var("key"))
        )),
        Body(Seq(
          Eq(ScalaTerm(s"(map: Map[$keyTy, $valueTy]) => map.nonEmpty", ScalaType.bool, Seq(Var("map"))), TRUE),
          Call(memRelName, Seq(Var("key").arg, ScalaTerm(s"(map: Map[$keyTy, $valueTy]) => map.tail", ScalaType(s"Map[$keyTy, $valueTy]"), Seq(Var("map"))).arg))
        ))
      ))
      mapMembershipRelations += memRel
      Seq(Call(memRelName, Seq(visitTerm(key).head.arg, visitTerm(map).head.arg)))
    case _ => super.visitAtom(atom)
  }

  override def visitAggregationOperator(op: AggregationOperator): AggregationOperator = op match
    case MonoAggregationOperator(MapMonoDefinition(keyTy, mono)) =>
      val kt = getSTName(keyTy)
      val inputVTy = getSTName(mono.typ.in)
      val stateVTy = getSTName(mono.typ.state)
      val outputVTy = getSTName(mono.typ.out)
      val valueAggOp = visitAggregationOperator(MonoAggregationOperator(mono)).asInstanceOf[ScalaMonoAggregationOperator]
      val initCode = valueAggOp.initCode
      val addCode = valueAggOp.addCode
      ScalaMonoAggregationOperator(
        name = s"ScalaMapMonoAggregation_${keyTy}_${mono.name}",
        inputTy = ScalaType(s"($kt, $inputVTy)"),
        stateTy = ScalaType(s"Map[$kt, $stateVTy]"),
        initCode = s"Map[$kt, $stateVTy]()",
        addCode =
          s"""(st: Map[$kt, $stateVTy], a: ($kt, $inputVTy)) =>
             | if st.contains(a._1) then st + (a._1 -> ($addCode (st(a._1), a._2)))
             | else st + (a._1 -> $addCode($initCode, a._2))
             |""".stripMargin
      )
    case _ => super.visitAggregationOperator(op)
