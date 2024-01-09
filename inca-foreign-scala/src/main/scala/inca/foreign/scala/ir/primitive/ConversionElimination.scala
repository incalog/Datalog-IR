package inca.foreign.scala.ir.primitive

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.bool.{AtomAsBool, TBoolean}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign}
import inca.ir.extension.*
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.extension.arithmetic.{TDouble, TInt}
import inca.ir.extension.data.TData
import inca.ir.extension.set.{SetComprehension, SetMember, TSet}
import inca.ir.extension.map.{MapComprehension, MapContains, MapLookUp, TMap}
import inca.ir.extension.string.TString
import inca.ir.extension.tuple.{Project, TTuple, TupleLit}
import inca.ir.lowering.BaseLowering

trait ConversionElimination extends BaseLowering:
  override val name: String = "ConversionElimination"
  def loweredIRs: Set[BaseIR] = Set(foreign.IR)
  def requiredIRs: Set[BaseIR] = Set(bool.IR, set.IR)

  protected def createRelName(name: String): Name =
    gensym.freshName(
      Seq("(", ")", "[", "]", ", ").foldLeft(name)((s, t) => s.replace(t, "$"))
    )

  var setMembershipRelations: Map[ScalaType, Relation] = _
  var scalasetMembershipRelations: Map[TSet, Relation] = _

  var mapMembershipRelations: Map[ScalaType, Relation] = _
  var scalamapMembershipRelations: Map[TMap, Relation] = _


  override def visitModule(module: Module): Module =
    setMembershipRelations = Map()
    scalasetMembershipRelations = Map()
    mapMembershipRelations = Map()
    scalamapMembershipRelations = Map()
    val mod = super.visitModule(module)
    mod.copy(contents = mod.contents
      ++ setMembershipRelations.values
      ++ scalasetMembershipRelations.values
      ++ mapMembershipRelations.values
      ++ scalamapMembershipRelations.values
    )

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case ConvertForeignIR(term, ty1, ty2) if ty1 == ty2 => Seq(term)
    case ConvertForeignIR(term, ScalaType("Boolean"), TBoolean) =>
      Seq(AtomAsBool(Eq(term, ScalaConstantTerm("true", ScalaType("Boolean")))))
    case ConvertForeignIR(term, ScalaType("Int"), TInt) =>
      Seq(Cast(term, TInt))
    case ConvertForeignIR(term, ScalaType("Double"), TDouble) =>
      Seq(Cast(term, TDouble))
    case ConvertForeignIR(term, ScalaType(nm1), TData(RefByName(Name(nm2)))) if nm1 == nm2 =>
      Seq(Cast(term, TData(nm2)))
    case ConvertForeignIR(term, ScalaType("String"), TString) => Seq(Cast(term, TString))
    case ConvertForeignIR(term, ScalaType(s"Set[$fty]"), TSet(irty)) =>
      // create a relation that enumerates all items in the set
      val setTy = s"Set[$fty]"
      val memRelName = createRelName(s"ScalaSetToSet$$$fty")
      val memRel = Relation(memRelName,
        Seq(
          Param("elem", ScalaType(fty)),
          Param("s", TDemand(ScalaType(setTy)))
        ),
        Seq(
          Body(Seq(
            Eq(ScalaTerm(s"(s: $setTy) => s.nonEmpty", ScalaType.bool, Seq(Var("s"))), ScalaConstantTerm.TRUE),
            Eq(ScalaTerm(s"(s: $setTy) => s.head", ScalaType(fty), Seq(Var("s"))), Var("elem"))
          )),
          Body(Seq(
            Eq(ScalaTerm(s"(s: $setTy) => s.nonEmpty", ScalaType.bool, Seq(Var("s"))), ScalaConstantTerm.TRUE),
            Call(memRelName, Seq(Var("elem").arg, ScalaTerm(s"(s: $setTy) => s.tail", ScalaType(setTy), Seq(Var("s"))).arg))
          ))
        )
      )
      setMembershipRelations += ScalaType(setTy) -> memRel
      val elem = Name(gensym.fresh("elem"))
      val set = SetComprehension(
        ConvertForeignIR(Var(elem), ScalaType(fty), irty),
        Seq(Call(memRelName, Seq(Var(elem).arg, term.arg)))
      )
      visitTerm(set)
    case ConvertForeignIR(term, stup@ScalaType(s"($styStr)"), TTuple(tys)) =>
      val stys = styStr.split(',').toSeq.map(_.trim)
      Seq(
        TupleLit.make(stys.zip(tys).zipWithIndex.flatMap { case ((sty,ty), ix) =>
          val proj = ScalaTerm(s"(x:${stup.name}) => x._${ix+1}", ScalaType(sty), Seq(term))
          visitTerm(ConvertForeignIR(proj, ScalaType(sty), ty))
        })
      )
    case ConvertForeignIR(term, smap@ScalaType(s"Map[$fkTy, $fvTy]"), TMap(irkTy, irvTy)) =>
      val memRelName = createRelName(s"ScalaMapToMap$$$irkTy$$$irvTy")
      val mapTy = s"Map[$fkTy, $fvTy]"
      val memRel = Relation(
        memRelName,
        Seq(
          Param("key", ScalaType(fkTy)),
          Param("value", ScalaType(fvTy)),
          Param("map", TDemand(ScalaType(mapTy)))
        ),
        Seq(
          Body(Seq(
            Eq(ScalaTerm(s"(map: $mapTy) => map.nonEmpty", ScalaType.bool, Seq(Var("map"))), ScalaConstantTerm.TRUE),
            Eq(ScalaTerm(s"(map: $mapTy) => map.head._1", ScalaType(fkTy), Seq(Var("map"))), Var("key")),
            Eq(ScalaTerm(s"(map: $mapTy) => map.head._2", ScalaType(fvTy), Seq(Var("map"))), Var("value"))
          )),
          Body(Seq(
            Eq(ScalaTerm(s"(map: $mapTy) => map.nonEmpty", ScalaType.bool, Seq(Var("map"))), ScalaConstantTerm.TRUE),
            Call(memRelName, Seq(Var("key").arg, Var("value").arg, ScalaTerm(s"(map: $mapTy) => map.tail", ScalaType(mapTy), Seq(Var("map"))).arg))
          ))
        )
      )
      mapMembershipRelations += ScalaType(mapTy) -> memRel
      val key = Name(gensym.freshName("key"))
      val value = Name(gensym.freshName("value"))
      val map = MapComprehension(
        ConvertForeignIR(Var(key), ScalaType(fkTy), irkTy),
        ConvertForeignIR(Var(value), ScalaType(fvTy), irvTy),
        Seq(Call(memRelName, Seq(Var(key).arg, Var(value).arg, term.arg)))
      )
      visitTerm(map)
    case ConvertForeignIR(term, ScalaType("Any"), TAny) =>
      Seq(Cast(term, TAny))
    case ConvertForeignIR(term, ScalaType(ty1), ty2) =>
      throw new UnsupportedOperationException(s"Cannot convert ScalaType $ty1 to $ty2")

    case ConvertIRForeign(term, ty1, ty2) if ty1 == ty2 => Seq(term)
    case ConvertIRForeign(term, TInt, ScalaType("Int")) =>
      Seq(Cast(term, ScalaType("Int")))
    case ConvertIRForeign(term, TDouble, ScalaType("Double")) =>
      Seq(Cast(term, ScalaType("Double")))
    case ConvertIRForeign(term, TString, ScalaType("String")) =>
      Seq(Cast(term, ScalaType("String")))
    case ConvertIRForeign(term, TAny, ScalaType("Any")) =>
      Seq(Cast(term, ScalaType("Any")))
    case ConvertIRForeign(term, TData(RefByName(Name(nm1))), ScalaType(nm2)) if nm1 == nm2 =>
      Seq(Cast(term, ScalaType(nm2)))
    case ConvertIRForeign(term, TBoolean, ScalaType("Boolean")) =>
      Seq(ScalaTerm("(x: Int) => x != 0", ScalaType("Boolean"), Seq(term)))
    case ConvertIRForeign(term, TSet(irty), ScalaType(s"Set[$fty]")) =>
      val setTy = s"Set[$fty]"
      val memRelName = createRelName(s"SetToScalaSet$$$fty")
      val Seq(memRel) = visitRelation(
        Relation(memRelName,
          Seq(
            Param("elem", ScalaType(fty)),
            Param("s", TDemand(TSet(irty)))
          ),
          Seq(Body(Seq(
            SetMember(Var("elemIR"), Var("s")),
            Eq(Var("elem"), ConvertIRForeign(Var("elemIR"), irty, ScalaType(fty)))
          )))
        )
      )
      val op = ScalaMonoAggregationOperator(
        Name(s"ScalaSetMono$$$fty"),
        ScalaType(fty),
        ScalaType(s"Set[$fty]"),
        initCode = s"Set[$fty]()",
        addCode = s"(st: Set[$fty], a: $fty) => st + a"
      )
      scalasetMembershipRelations += TSet(irty) -> memRel
      val elem = Name(gensym.fresh("elem"))
      Seq(block.Block(
        aggregate.Aggregate(RefByName(memRelName), Seq(AggregateColumnArg(Var(elem)), term.arg), op),
        Var(elem)
      ))
    case ConvertIRForeign(term, TTuple(tys), stup@ScalaType(s"($styStr)")) =>
      val stys = styStr.split(',').toSeq.map(_.trim)
      val params = stys.zipWithIndex.map((sty, ix) => s"x$ix: $sty").mkString("(", ", ", ")")
      val tuple = stys.indices.map(ix => s"x$ix").mkString("(", ", ", ")")
      val argsWithConvert = stys.indices.map(ix => ConvertIRForeign(Project(term, ix), tys(ix), ScalaType(stys(ix))))
      val args = argsWithConvert.flatMap(visitTerm)
      Seq(
        ScalaTerm(s"$params => $tuple", stup, args)
      )
    case ConvertIRForeign(term, TMap(irkTy, irvTy), smap@ScalaType(s"Map[$fkTy, $fvTy]")) =>
      val mapTy = s"Map[$fkTy, $fvTy]"
      val memRelName = createRelName(s"MapToScalaMap$$$fkTy$$$fvTy")
      val Seq(memRel) = visitRelation(
        Relation(memRelName,
          Seq(
            Param("kvPair", ScalaType(s"($fkTy, $fvTy)")),
            Param("map", TDemand(TMap(irkTy, irvTy)))
          ),
          Seq(Body(Seq(
            MapContains(Var("keyIR"), Var("map")),
            Eq(Var("valueIR"), MapLookUp(Var("map"), Var("keyIR"))),
            Eq(Var("kvPair"), ConvertIRForeign(
              TupleLit(Seq(Var("keyIR"), Var("valueIR"))),
              TTuple(Seq(irkTy, irvTy)),
              ScalaType(s"($fkTy, $fvTy)")
            ))
          )))
        )
      )
      val skvTy = s"($fkTy, $fvTy)"
      val smapTy = s"Map[$fkTy, $fvTy]"
      val op = ScalaMonoAggregationOperator(
        Name(s"ScalaMapMono$$$fkTy$$$fvTy"),
        ScalaType(skvTy),
        ScalaType(smapTy),
        initCode = s"$smapTy()",
        addCode = s"(st: $smapTy, a: $skvTy) => st + (a._1 -> a._2)"
      )
      scalamapMembershipRelations += TMap(irkTy, irvTy) -> memRel
      val map = Name(gensym.fresh("map"))
      Seq(block.Block(
        aggregate.Aggregate(RefByName(memRelName), Seq(AggregateColumnArg(Var(map)), term.arg), op),
        Var(map)
      ))
    case ConvertIRForeign(term, ty1, ScalaType(ty2)) =>
      throw new UnsupportedOperationException(s"Cannot convert $ty1 to ScalaType $ty2")

    case _ => super.visitTerm(term)
  }