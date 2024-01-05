package inca.foreign.scala.ir.mono

import inca.foreign.scala.ir.primitive.{ScalaConstantTerm, ScalaInca, ScalaMonoAggregationOperator, ScalaType, ScalaLowering as BaseScalaLowering}
import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg, AggregationOperator}
import inca.ir.extension.demand.{DemandIgnoreCallHint, TDemand}
import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign}
import inca.ir.extension.mono.{MapMonoDefinition, MonoAggregationOperator, NaiveSetMonoDefinition, SetMonoDefinition2}
import inca.ir.extension.set.TSet
import inca.ir.extension.tuple.TTuple
import inca.ir.extension.{block, demand}

import scala.collection.mutable.ListBuffer


trait ScalaLowering extends BaseScalaLowering:

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
      var newAgg = Aggregate(rel, args.flatMap(visitArg), visitAggregationOperator(op))
      var suffix = Seq[Atom]()
      val Seq(aggIndex) = agg.aggregationColumns

      inputConversion.foreach { (from, to) =>
        // TODO generate new collect relation using inputConversion
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
    case MonoAggregationOperator(SetMonoDefinition2(ty)) =>
      val sty = ScalaInca.compileType(ty)
      val styName = sty.name.replace("[", "$").replace("]", "$")
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
    case MonoAggregationOperator(NaiveSetMonoDefinition(ty)) =>
      val sty = getSTName(ty)
      ScalaMonoAggregationOperator(
        Name(s"ScalaNaiveSetMono$$${sty.replace("[", "$").replace("]", "$")}"),
        ScalaType(sty),
        ScalaType(s"Set[$sty]"),
        initCode = s"Set[$sty]()",
        addCode = s"(st: Set[$sty], a: $sty) => st + a"
      )
    case MonoAggregationOperator(mm@MapMonoDefinition(keyTy, mono)) =>
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
        outputTy = ScalaType(s"Map[$kt, $stateVTy]"),
        initCode = s"Map[$kt, $stateVTy]()",
        addCode =
          s"""(st: Map[$kt, $stateVTy], a: ($kt, $inputVTy)) =>
             | if st.contains(a._1) then st + (a._1 -> ($addCode (st(a._1), a._2)))
             | else st + (a._1 -> $addCode($initCode, a._2))
             |""".stripMargin
      )
    case _ => super.visitAggregationOperator(op)

def scalaSetMonoDefinition(ty: Type): SetMonoDefinition2 = {
  val rtTy = ScalaInca.compileType(ty)
  val scalaSetType = ScalaType(s"Set[${rtTy.name}]")
  SetMonoDefinition2(ty)
}

