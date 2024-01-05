package inca.foreign.scala.ir.primitive

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.bool.{AtomAsBool, TBoolean}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.foreign.ConvertForeignIR
import inca.ir.extension.*
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.data.TData
import inca.ir.extension.set.{SetComprehension, TSet}
import inca.ir.extension.string.TString
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
      Seq(Cast(term, TData.apply(nm2)))
    case ConvertForeignIR(term, ScalaType("String"), TString) => Seq(Cast(term, TString))
    case _ => super.visitTerm(term)
  }