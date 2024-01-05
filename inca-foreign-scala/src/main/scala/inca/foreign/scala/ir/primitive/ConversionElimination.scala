package inca.foreign.scala.ir.primitive

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.bool.{AtomAsBool, TBoolean}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.foreign.{ConvertForeignIR, ConvertIRForeign}
import inca.ir.extension.*
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.data.TData
import inca.ir.extension.set.{SetComprehension, TSet}
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

  var setMembershipRelations: Map[ScalaType, Relation] = Map()

  override def visitModule(module: Module): Module =
    setMembershipRelations = Map()
    val mod = super.visitModule(module)
    mod.copy(contents = mod.contents ++ setMembershipRelations.values)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case ConvertForeignIR(term, ty1, ty2) if ty1 == ty2 => Seq(term)
    case ConvertForeignIR(term, ScalaType("Boolean"), TBoolean) =>
      Seq(AtomAsBool(Eq(term, ScalaConstantTerm("true", ScalaType("Boolean")))))
    case ConvertForeignIR(term, ScalaType("Int"), TInt) =>
      Seq(Cast(term, TInt))
    case ConvertForeignIR(term, ScalaType(s"Set[$fty]"), TSet(irty)) =>
      // create a relation that enumerates all items in the set
      val setTy = s"Set[$fty]"
      val memRelName = createRelName(s"Set$$Convert$$$fty")
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
    case ConvertForeignIR(term, ScalaType(nm1), TData(RefByName(Name(nm2)))) if nm1 == nm2 =>
      Seq(Cast(term, TData(nm2)))
    case ConvertForeignIR(term, ScalaType("String"), TString) => Seq(Cast(term, TString))
    case ConvertForeignIR(term, stup@ScalaType(s"($styStr)"), TTuple(tys)) =>
      val stys = styStr.split(',').toSeq.map(_.trim)
      Seq(
        TupleLit.make(stys.zip(tys).zipWithIndex.flatMap { case ((sty,ty), ix) =>
          val proj = ScalaTerm(s"(x:${stup.name}) => x._${ix+1}", ScalaType(sty), Seq(term))
          visitTerm(ConvertForeignIR(proj, ScalaType(sty), ty))
        })
      )
    case ConvertForeignIR(term, ty1, ty2) =>
      ???

    case ConvertIRForeign(term, ty1, ty2) if ty1 == ty2 => Seq(term)
    case ConvertIRForeign(term, TInt, ScalaType("Int")) =>
      Seq(Cast(term, ScalaType("Int")))
    case ConvertIRForeign(term, TString, ScalaType("String")) =>
      Seq(Cast(term, ScalaType("String")))
    case ConvertIRForeign(term, TData(RefByName(Name(nm1))), ScalaType(nm2)) if nm1 == nm2 =>
      Seq(Cast(term, ScalaType(nm2)))
    case ConvertIRForeign(term, TBoolean, ScalaType("Boolean")) =>
      Seq(ScalaTerm("(x: Int) => x != 0", ScalaType("Boolean"), Seq(term)))
    case ConvertIRForeign(term, TTuple(tys), stup@ScalaType(s"($styStr)")) =>
      val stys = styStr.split(',').toSeq.map(_.trim)
      val params = stys.zipWithIndex.map((sty, ix) => s"x$ix: $sty").mkString("(", ", ", ")")
      val tuple = stys.indices.map(ix => s"x$ix").mkString("(", ", ", ")")
      val args = stys.indices.map(ix => Project(term, ix))
      Seq(
        ScalaTerm(s"$params => $tuple", stup, args)
      )
    case ConvertIRForeign(term, ty1, ty2) => ???

    case _ => super.visitTerm(term)
  }