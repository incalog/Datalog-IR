package inca.foreign.scala.ir.mono

import inca.foreign.scala.ir.primitive
import inca.foreign.scala.ir.primitive.{ScalaInca, ScalaMonoAggregationOperator, ScalaMonoDefinition, ScalaType}
import inca.ir
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg, AggregationOperator}
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.demand.{DemandIgnoreCallHint, TDemand}
import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign}
import inca.ir.extension.map.TMap
import inca.ir.extension.{aggregate, demand, foreign, mono, set}
import inca.ir.extension.mono.{ArithmeticMonoDefinition, MapMonoDefinition, MonoAggregationOperator, SetMonoDefinition, StringMonoDefinition}
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

  override def visitModule(module: Module): Module =
    val mod = super.visitModule(module)
    mod.copy(contents = mod.contents ++ convertRelations)

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
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

  override def visitAggregationOperator(op: AggregationOperator): AggregationOperator = op match
    case MonoAggregationOperator(ArithmeticMonoDefinition.SumInt) =>
      inputConversion = Some((TInt, ScalaType.int))
      outputConversion = Some((ScalaType.int, TInt))
      ScalaMonoAggregationOperator(
        name = "Sum Int Mono",
        inputTy = ScalaType.int,
        outputTy = ScalaType.int,
        initCode = "0",
        addCode = "(x:Int,y:Int) => x + y"
      )
    case MonoAggregationOperator(ArithmeticMonoDefinition.SumDouble) =>
      inputConversion = Some((TDouble, ScalaType.double))
      outputConversion = Some((ScalaType.double, TDouble))
      ScalaMonoAggregationOperator(
        name = "Sum Double Mono",
        inputTy = ScalaType.double,
        outputTy = ScalaType.double,
        initCode = "0",
        addCode = "(x:Double,y:Double) => x + y"
      )
    case MonoAggregationOperator(ArithmeticMonoDefinition.MaxInt) =>
      inputConversion = Some((TInt, ScalaType.int))
      outputConversion = Some((ScalaType.int, TInt))
      ScalaMonoAggregationOperator(
        name = "Max Int Mono",
        inputTy = ScalaType.int,
        outputTy = ScalaType.int,
        initCode = "Int.MinValue",
        addCode = "(x:Int,y:Int) => x max y",
      )
    case MonoAggregationOperator(ArithmeticMonoDefinition.MaxDouble) =>
      inputConversion = Some((TDouble, ScalaType.double))
      outputConversion = Some((ScalaType.double, TDouble))
      ScalaMonoAggregationOperator(
        name = "Max Double Mono",
        inputTy = ScalaType.double,
        outputTy = ScalaType.double,
        initCode = "Double.NegativeInfinity",
        addCode = "(x:Double,y:Double) => x max y",
      )
    case MonoAggregationOperator(ArithmeticMonoDefinition.Count) =>
      inputConversion = Some((TAny, ScalaType.any))
      outputConversion = Some((ScalaType.int, TInt))
      ScalaMonoAggregationOperator(
        name = "Count Mono",
        inputTy = ScalaType.any,
        outputTy = ScalaType.int,
        initCode = "0",
        addCode = "(st: Int, a: Any) => st + 1"
      )
    case MonoAggregationOperator(StringMonoDefinition) =>
      inputConversion = Some((TString, ScalaType.string))
      outputConversion = Some((ScalaType.string, TString))
      ScalaMonoAggregationOperator(
        name = "String Mono",
        inputTy = ScalaType.string,
        outputTy = ScalaType.string,
        initCode = """""""",
        addCode = "(st: String, a: String) => st + a"
      )

    case MonoAggregationOperator(SetMonoDefinition(ty)) =>
      val sty = ScalaInca.compileType(ty)
      val styName = sty.name
      val scalaSet = ScalaType(s"Set[$styName]")

      inputConversion = Some((ty, sty))
      outputConversion = Some((scalaSet, TSet(ty)))

      ScalaMonoAggregationOperator(
        Name(s"ScalaSetMono$$$styName"),
        sty,
        scalaSet,
        initCode = s"Set[$styName]()",
        addCode = s"(st: Set[$styName], a: $styName) => st + a"
      )
    case MonoAggregationOperator(mm@MapMonoDefinition(keyTy, mono)) =>
      val kt = ScalaInca.compileType(keyTy).name // scala type of key
      val inputVSTy = ScalaInca.compileType(mono.typ.in).name 
      val stateVSTy = ScalaInca.compileType(mono.typ.state).name
      val outputVSTy = ScalaInca.compileType(mono.typ.out).name
      val valueAggOp = visitAggregationOperator(MonoAggregationOperator(mono)).asInstanceOf[ScalaMonoAggregationOperator]
      val initCode = valueAggOp.initCode
      val addCode = valueAggOp.addCode
      val inputMMSTy = ScalaInca.compileType(mm.typ.in).name // scala type of value mono's input
      val stateMMSTy = ScalaInca.compileType(mm.typ.state).name // scala type of value mono's state
      val outputMMSTy = ScalaInca.compileType(mm.typ.out).name // scala type value mono's output
      inputConversion = Some((mm.typ.in, ScalaType(inputMMSTy)))
      outputConversion = Some((ScalaType(stateMMSTy), mm.typ.state))
      ScalaMonoAggregationOperator(
        name = s"ScalaMapMonoAggregation_${keyTy}_${mono.name}",
        inputTy = ScalaType(inputMMSTy),
        outputTy = ScalaType(stateMMSTy),
        initCode = s"$stateMMSTy()",
        addCode =
          s"""(st: $stateMMSTy, a: $inputMMSTy) =>
             | if st.contains(a._1) then st + (a._1 -> ($addCode)(st(a._1), a._2))
             | else st + (a._1 -> ($addCode)($initCode, a._2))
             |""".stripMargin
      )
    // user-defined mono
    case MonoAggregationOperator(ScalaMonoDefinition(name, initCode, addCode, resultCode, constructorParamTypes, typ)) =>
      val inputType = ScalaInca.compileType(typ.in)
      val outputType = ScalaInca.compileType(typ.state)
      inputConversion = Some((typ.in, inputType))
      outputConversion = Some((outputType, typ.state))
      ScalaMonoAggregationOperator(name, inputType, outputType, initCode, addCode)
    case _ => super.visitAggregationOperator(op)

def scalaSetMonoDefinition(ty: Type): SetMonoDefinition = {
  val rtTy = ScalaInca.compileType(ty)
  val scalaSetType = ScalaType(s"Set[${rtTy.name}]")
  SetMonoDefinition(ty)
}

