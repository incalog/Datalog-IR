package inca.foreign.scala.ir.set

import inca.foreign.scala.ir.primitive.ScalaConstantTerm.TRUE
import inca.foreign.scala.ir.primitive.{ScalaAggregationAtom, ScalaAggregationOperator, ScalaConstantTerm, ScalaInca, ScalaMonoAggregationOperator, ScalaTerm, ScalaType, ScalaLowering as BaseScalaLowering}
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, TSet, IR as setIR}
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Module, Name, Param, Relation, Term, Type, Var, WildcardArg, string2name}
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.block.Block
import inca.ir.extension.demand.TDemand
import inca.ir.extension.foreign.ConvertForeignIR
import inca.ir.extension.{block, demand}
import inca.ir.extension.mono.{MonoAggregationOperator, NaiveSetMonoDefinition, SetMonoDefinition2}
import inca.ir.extension.tuple.{TTuple, TupleLit}


trait ScalaLowering extends BaseScalaLowering:
//  override def isTypeSupported(ty: Type): Boolean = ty match
//    case TSet(ty) => true
//    case _ => super.isTypeSupported(ty)
//
//  private var setCompCollRelations: Set[Relation] = _
//  private var setMembershipRelations: Set[Relation] = _
//  private var setCompAggCounter: Int = _
//  private var currentModule: Module = _
//
//  private def createSetAggOp(name: Name, elemTy: String): ScalaMonoAggregationOperator =
//    ScalaMonoAggregationOperator(
//      name = name,
//      inputTy = ScalaType(s"$elemTy"),
//      stateTy = ScalaType(s"Set[$elemTy]"),
//      initCode = s"Set[$elemTy]()",
//      addCode = s"(st: Set[$elemTy], a: $elemTy) => st + a"
//    )
//
//  override def visitModule(module: Module): Module =
//    setCompAggCounter = 0
//    setCompCollRelations = Set()
//    setMembershipRelations = Set()
//    currentModule = module
//    val mod = super.visitModule(module)
//    mod.copy(contents = mod.contents ++ setCompCollRelations ++ setMembershipRelations)

//  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
//    case SetLit(ts) =>
//      val setTy = term.typ.get.ty.asInstanceOf[TSet]
//      val elemTy = visitType(setTy.ty).asInstanceOf[ScalaType]
//      val sts = ts.flatMap(visitTerm)
//      val sty = visitType(setTy).asInstanceOf[ScalaType]
//      val params = sts.zipWithIndex map {case (t, i) => s"v$i: ${elemTy.name}"}
//      val lits = sts.indices map (i => s"v$i")
//      Seq(ScalaTerm(s"(${params.mkString(", ")}) => Set(${lits mkString ", "})", sty, sts))
//    case SetUnion(t1, t2) =>
//      val st1 = visitTerm(t1).head
//      val st2 = visitTerm(t2).head
//      val setTy = visitType(t1.typ.get.ty).asInstanceOf[ScalaType].name
//      Seq(ScalaTerm(s"(s1: $setTy, s2: $setTy) => s1 | s2", ScalaType(setTy), Seq(st1, st2)))
//    case SetIntersection(t1, t2) =>
//      val st1 = visitTerm(t1).head
//      val st2 = visitTerm(t2).head
//      val setTy = visitType(t1.typ.get.ty).asInstanceOf[ScalaType].name
//      Seq(ScalaTerm(s"(s1: $setTy, s2: $setTy) => s1 & s2", ScalaType(setTy), Seq(st1, st2)))
//    case SetComprehension(elem, atoms) =>
//      val elemITy = elem.typ.get.ty // inca type
//      val elemSTy = ScalaInca.compileType(elemITy) // scala type
//      val collNm = gensym.freshName("set$comp$elem")
//      // collect all free variables
//      val vars = term.vars
//      vars.foreach(v => v.typ.getOrElse(throw new IllegalStateException(s"Set lowering requires types IR in $v")))
//      val (boundVars, bindingVars) = vars.partition(!_.typ.get.mode.isBinding)
//      val freeVars = boundVars.toSet diff bindingVars.toSet
//      val inputParams = freeVars.toSeq.map(v => v.name -> TDemand(ScalaInca.compileType(v.typ.get.ty)))
//      val ats = atoms.flatMap(visitAtom)
//      val Seq(visitedElem) = visitTerm(elem)
//      setCompAggCounter += 1
//      setCompCollRelations += Relation(s"coll$$set$$comp$$$setCompAggCounter", inputParams.map((nm, ty) => Param(nm, ty)) :+ Param(collNm, elemSTy), Seq(Body(ats :+ Eq(Var(collNm), visitedElem))))
//      val aggOp = createSetAggOp(s"AggOp$$Set$$${elemSTy.name}", elemSTy.name)
//      val resNm = gensym.freshName("set$comp$res")
//      val aggAtom = ScalaAggregationAtom(aggOp, s"coll$$set$$comp$$$setCompAggCounter", Var(resNm), inputParams.map((nm, _) => Var(nm)) :+ Var(gensym.freshName("_")), inputParams.size)
//      val callAtom = Call(s"coll$$set$$comp$$$setCompAggCounter", inputParams.map((nm, _) => Var(nm).arg) :+ Var(gensym.freshName("elem")).arg)
//      Seq(Block(Seq(callAtom, aggAtom), Var(resNm)))
//    case SetFrom(name) =>
//      val Seq(rel) = visitRelation(currentModule.relations(name.name))
//      val elemITy = TTuple.make(rel.params.map(_.ty)) // TODO: consider how to handle demanded parameters
//      val elemSTy = visitType(elemITy).asInstanceOf[ScalaType].name
//      val collNm = s"set$$from$$${rel.name}"
//      val args = rel.params.indices.map(i => Var(s"v$i"))
//      val scalaTpl = ScalaTerm(
//        s"(${rel.params.zipWithIndex.map((p, i) => s"v$i: ${p.ty.asInstanceOf[ScalaType].name}").mkString(", ")}) => (${args.map(v => v.name).mkString(", ")})",
//        ScalaType(elemSTy),
//        args
//      )
//      val collRel = Relation(
//        collNm,
//        Seq(Param("elem", ScalaType(elemSTy))),
//        Seq(Body(Seq(
//          Call(name, args.map(_.arg)),
//          Eq(scalaTpl, Var("elem"))
//        )))
//      )
//      val aggOp = createSetAggOp(s"AggOp$$Set$$${elemSTy.name}", elemSTy.name)
//      val resNm = gensym.freshName("set$from$res")
//      val aggAtom = ScalaAggregationAtom(aggOp, collNm, Var(resNm), Seq(Var(gensym.freshName("_"))), 0)
//      setCompCollRelations += collRel
//      Seq(Block(Seq(aggAtom), Var(resNm)))
//    case _ => super.visitTerm(term)
//  }

//  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
//    case SetMember(mem, s) =>
//      // create a relation that enumerates all items in the set
//      // (applying set contains method at here is not appropriate because
//      // `i` is an unbounded variable in the semantics of atom SetMember(i, s).
//      val memTy = ScalaInca.compileType(mem.typ.get.ty).name
//      val setTy = ScalaInca.compileType(s.typ.get.ty).name
//      val memRelName = createRelName(s"Set$$Mem$$$memTy")
//      val memRel = Relation(memRelName, Seq(Param("elem", ScalaType(memTy)), Param("s", TDemand(ScalaType(setTy)))), Seq(
//        Body(Seq(
//          Eq(ScalaTerm(s"(s: $setTy) => s.nonEmpty", ScalaType.bool, Seq(Var("s"))), TRUE),
//          Eq(ScalaTerm(s"(s: $setTy) => s.head", ScalaType(memTy), Seq(Var("s"))), Var("elem"))
//        )),
//        Body(Seq(
//          Eq(ScalaTerm(s"(s: $setTy) => s.nonEmpty", ScalaType.bool, Seq(Var("s"))), TRUE),
//          Call(memRelName, Seq(Var("elem").arg, ScalaTerm(s"(s: $setTy) => s.tail", ScalaType(setTy), Seq(Var("s"))).arg))
//        ))
//      ))
//      setMembershipRelations += memRel
//      Seq(Call(memRelName, Seq(visitTerm(mem).head.arg, visitTerm(s).head.arg)))
//    case _ => super.visitAtom(atom)
//  }

  override def visitAggregationOperator(op: AggregationOperator): AggregationOperator = op match
    case MonoAggregationOperator(SetMonoDefinition2(ty, rty)) =>
      val sty = visitType(ty).asInstanceOf[ScalaType].name
      ScalaMonoAggregationOperator(
        Name(s"ScalaSetMono$$${sty.replace("[", "$").replace("]", "$")}"),
        ScalaType(sty),
        ScalaType(s"Set[$sty]"),
        initCode = s"Set[$sty]()",
        addCode = s"(st: Set[$sty], a: $sty) => st + a"
      )
    case MonoAggregationOperator(NaiveSetMonoDefinition(ty)) =>
      val sty = visitType(ty).asInstanceOf[ScalaType].name
      ScalaMonoAggregationOperator(
        Name(s"ScalaNaiveSetMono$$${sty.replace("[", "$").replace("]", "$")}"),
        ScalaType(sty),
        ScalaType(s"Set[$sty]"),
        initCode = s"Set[$sty]()",
        addCode = s"(st: Set[$sty], a: $sty) => st + a"
      )
    case _ => super.visitAggregationOperator(op)

def scalaSetMonoDefinition(ty: Type): SetMonoDefinition2 = {
  val scalaSetType = ScalaType(s"Set[${ScalaInca.compileType(ty).name}]")
  SetMonoDefinition2(ty, scalaSetType)
}
