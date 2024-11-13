package inca.foreign.scala.ir.mono

import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.{ScalaInca, ScalaMonoAggregationOperator, ScalaMonoDefinition, ScalaType}
import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg, AggregationOperator}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.bool.TBoolean
import inca.ir.extension.demand.{DemandIgnoreCallHint, TDemand}
import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign}
import inca.ir.extension.map.TMap
import inca.ir.extension.{aggregate, demand, foreign, mono, set}
import inca.ir.extension.mono.{ArithmeticMonoDefinition, DisjMonoDefinition, MapMonoDefinition, MonoAggregationOperator, MonoDefinition, SetMonoDefinition, StringConcatMonoDefinition}
import inca.ir.extension.set.TSet
import inca.ir.extension.string.TString
import inca.ir.extension.tuple.TTuple
import inca.ir.lowering.BaseLowering


trait MonoLowering extends BaseLowering with primitive.Visitor:

  override def name: String = "MonoScalaLowering"

  override def loweredIRs: Set[BaseIR] = Set(mono.IR, aggregate.IR)

  override def requiredIRs: Set[BaseIR] = Set(set.IR, demand.IR, foreign.IR, aggregate.IR)

  /** Return the scala type name of input inca type */
  inline private def getSTName(ty: Type): String = visitType(ty) match
    case sty: ScalaType => sty.name
    case _ => throw new IllegalAccessError(s"Expected a ScalaType, but got $ty")

  var inputConversion: Option[(Type, Type)] = None
  var outputConversion: Option[(Type, Type)] = None

  var convertRelations: Set[Relation] = Set()

  override def visitModule(module: Module): Module = preserveHints(module) {
    val mod = super.visitModule(module)
    mod.copy(contents = mod.contents ++ convertRelations)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    atom match
      case agg@Aggregate(rel, args, op) =>
        val newArgs = args.flatMap(visitArg)
        var newAgg = Aggregate(rel, newArgs, visitAggregationOperator(op))
        var suffix = Seq[Atom]()
        val Seq(aggIndex) = agg.aggregationColumns

        inputConversion.foreach { (from, to) =>
          // generate new collect relation using inputConversion
          // foo(m: TDemand(A), input: TDemand(B)) { ... }
          // ~>
          // foo$$converted(m: A, converted: C) {
          //   foo(m,input)#ignore
          //   converted == convert(input)
          // }
          val relParams = rel.target.get.params.map(p =>
            Param(p.name, p.ty match
              case TDemand(ty) => ty
              case ty => ty
            )
          )
          val convertedParam = Name(gensym.fresh("converted"))
          val convertedParams = relParams.updated(aggIndex, Param(convertedParam, to))

          val convertedRel = Relation(gensym.freshName(rel.name + "$$converted"),
            convertedParams,
            Seq(Body(Seq(
              Call(rel, relParams.map(p => Var(p.name).arg), false).addHint(DemandIgnoreCallHint),
              Eq(Var(convertedParam.name), ConvertIRForeign(Var(relParams(aggIndex).name), from, to))
            )))
          )
          convertRelations += convertedRel
          newAgg = Aggregate(RefByName(convertedRel.name), newAgg.args, newAgg.op)
        }

        outputConversion.foreach { (from, to) =>
          // outputConversion:
          // aggregate(Collect(a,b,#c), op)
          // ~>
          // aggregate(Collect(a,b,#tmp), op)
          // c == convert(tmp)

          var output: Option[(Name, Term)] = None
          val internalArgs = agg.mapAggregateColumn { t =>
            val v = Name(gensym.fresh("convertAggOutput"))
            output = Some((v, t))
            AggregateColumnArg(Var(v))
          }

          newAgg = Aggregate(newAgg.rel, internalArgs, newAgg.op)
          suffix ++= output.map { case (name, term) =>
            Eq(term, ConvertForeignIR(Var(name), from, to))
          }
        }

        newAgg +: suffix


      case _ => super.visitAtom(atom)
  }

  def scalaMono(mono: MonoDefinition, initCode: String, addCode: String, resultCode: String, combineCode: String): ScalaMonoAggregationOperator = {
    val stateTy = ScalaInca.compileType(mono.typ.state).code
    val inputTy = ScalaInca.compileType(mono.typ.in).code
    val outputTy = ScalaInca.compileType(mono.typ.out).code
    ScalaMonoAggregationOperator(
      name = mono.name,
      stateTy = ScalaType(stateTy),
      inputTy = ScalaType(inputTy),
      outputTy = ScalaType(outputTy),
      initCode = initCode,
      addCode = s"(st:$stateTy,in:$inputTy) => $addCode",
      resultCode = s"(st:$stateTy) => $resultCode",
      combineCode = s"(o1:$outputTy, o2:$outputTy) => $combineCode"
    )
  }

  override def visitAggregationOperator(op: AggregationOperator): AggregationOperator = op match
    case MonoAggregationOperator(mono@ArithmeticMonoDefinition.SumInt) =>
      inputConversion = Some((TInt, ScalaType.int))
      outputConversion = Some((ScalaType.int, TInt))
      scalaMono(mono, "0", "st + in", "st", "o1 + o2")
    case MonoAggregationOperator(mono@ArithmeticMonoDefinition.SumDouble) =>
      inputConversion = Some((TDouble, ScalaType.double))
      outputConversion = Some((ScalaType.double, TDouble))
      scalaMono(mono, "0", "st + in", "st", "o1 + o2")
    case MonoAggregationOperator(mono@ArithmeticMonoDefinition.MaxInt) =>
      inputConversion = Some((TInt, ScalaType.int))
      outputConversion = Some((ScalaType.int, TInt))
      scalaMono(mono, "Int.MinValue", "st max in", "st", "o1 max o2")
    case MonoAggregationOperator(mono@ArithmeticMonoDefinition.MaxDouble) =>
      inputConversion = Some((TDouble, ScalaType.double))
      outputConversion = Some((ScalaType.double, TDouble))
      scalaMono(mono, "Double.NegativeInfinity", "st max in", "st", "o1 max o2")
    case MonoAggregationOperator(mono@ArithmeticMonoDefinition.Count) =>
      inputConversion = Some((TAny, ScalaType.any))
      outputConversion = Some((ScalaType.int, TInt))
      scalaMono(mono, "0", "st + 1", "st", "o1 + o2")
    case MonoAggregationOperator(mono@StringConcatMonoDefinition()) =>
      inputConversion = Some((TString, ScalaType.string))
      outputConversion = Some((ScalaType.string, TString))
      scalaMono(mono, "\"\"", "st + in", "st", "o1 + o2")
    case MonoAggregationOperator(mono@DisjMonoDefinition()) =>
      inputConversion = Some((TBoolean, ScalaType.bool))
      outputConversion = Some((ScalaType.bool, TBoolean))
      scalaMono(mono, "false", "st || in", "st", "o1 || o2")
    case MonoAggregationOperator(SetMonoDefinition(ty)) =>
      val sty = ScalaInca.compileType(ty)
      val styName = sty.name
      val scalaSet = ScalaType(s"Set[$styName]")

      inputConversion = Some((ty, sty))
      outputConversion = Some((scalaSet, TSet(ty)))

      ScalaMonoAggregationOperator(
        Name(s"ScalaSetMono$$$styName"),
        scalaSet,
        sty,
        scalaSet,
        initCode = s"Set[$styName]()",
        addCode = s"(st: Set[$styName], a: $styName) => st + a",
        resultCode = s"(st: Set[$styName]) => st",
        combineCode = s"(o1: Set[$styName], o2: Set[$styName]) => o1 ++ o2",
      )
    case MonoAggregationOperator(mm@MapMonoDefinition(keyTy, mono)) =>
      val kt = ScalaInca.compileType(keyTy).name // scala type of key
      val inputVSTy = ScalaInca.compileType(mono.typ.in).name
      val stateVSTy = ScalaInca.compileType(mono.typ.state).name
      val outputVSTy = ScalaInca.compileType(mono.typ.out).name
      val valueAggOp = visitAggregationOperator(MonoAggregationOperator(mono)).asInstanceOf[ScalaMonoAggregationOperator]
      val initCode = valueAggOp.initCode
      val addCode = valueAggOp.addCode
      val resultCode = valueAggOp.resultCode
      val combineCode = valueAggOp.combineCode
      val inputMMSTy = ScalaInca.compileType(mm.typ.in).name // scala type of value mono's input
      val stateMMSTy = ScalaInca.compileType(mm.typ.state).name // scala type of value mono's state
      val outputMMSTy = ScalaInca.compileType(mm.typ.out).name // scala type value mono's output
      inputConversion = Some((mm.typ.in, ScalaType(inputMMSTy)))
      outputConversion = Some((ScalaType(outputMMSTy), mm.typ.out))
      ScalaMonoAggregationOperator(
        name = s"ScalaMapMonoAggregation_${keyTy}_${mono.name}",
        stateTy = ScalaType(stateMMSTy),
        inputTy = ScalaType(inputMMSTy),
        outputTy = ScalaType(outputMMSTy),
        initCode = s"$stateMMSTy()",
        addCode =
          s"""(st: $stateMMSTy, a: $inputMMSTy) =>
             | if st.contains(a._1) then st + (a._1 -> ($addCode)(st(a._1), a._2))
             | else st + (a._1 -> ($addCode)($initCode, a._2))
             |""".stripMargin,
        resultCode = s"(st: $stateMMSTy) => st.mapValues(($resultCode)).toMap",
        combineCode =
          s"""(map1: $outputMMSTy, map2: $outputMMSTy) => {
             |  var result = map1
             |  for ((k, v1) <- map2)
             |    val v = map1.get(k) match
             |      case None => v1
             |      case Some(v2) => ($combineCode)(v1, v2)
             |    result += k -> v
             |  result
             |}""".stripMargin
      )
    // user-defined mono
    case MonoAggregationOperator(ScalaMonoDefinition(name, initCode, addCode, resultCode, combineCode, constructorParamTypes, typ)) =>
      val stateType = ScalaInca.compileType(typ.state)
      val inputType = ScalaInca.compileType(typ.in)
      val outputType = ScalaInca.compileType(typ.out)
      inputConversion = Some((typ.in, inputType))
      outputConversion = Some((outputType, typ.out))
      ScalaMonoAggregationOperator(name, stateType, inputType, outputType, initCode, addCode, resultCode, combineCode)
    case _ => super.visitAggregationOperator(op)

def scalaSetMonoDefinition(ty: Type): SetMonoDefinition = {
  val rtTy = ScalaInca.compileType(ty)
  val scalaSetType = ScalaType(s"Set[${rtTy.name}]")
  SetMonoDefinition(ty)
}

