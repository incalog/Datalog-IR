package inca.foreign.scala.ir.set

import inca.foreign.scala.ir.primitive.ScalaConstantTerm.TRUE
import inca.foreign.scala.ir.primitive.{ScalaAggregationAtom, ScalaAggregationOperator, ScalaConstantTerm, ScalaInca, ScalaMonoAggregationOperator, ScalaTerm, ScalaType, ScalaLowering as BaseScalaLowering}
import inca.ir.extension.set.{SetComprehension, SetFrom, SetIntersection, SetLit, SetMember, SetUnion, TSet, IR as setIR}
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Module, Name, Param, Relation, Term, Type, Var, WildcardArg, string2name}
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.AggregationOperator
import inca.ir.extension.block.Block
import inca.ir.extension.demand.TDemand
import inca.ir.extension.{demand, block}
import inca.ir.extension.mono.{MonoAggregationOperator, NaiveSetMonoDefinition}
import inca.ir.extension.tuple.{TTuple, TupleLit}


trait ScalaLowering extends BaseScalaLowering:
  override def name: String = "ScalaSet"

  override def loweredIRs: Set[BaseIR] = Set(setIR)
  override def requiredIRs: Set[BaseIR] = Set(demand.IR, block.IR)

  override def isTypeSupported(ty: Type): Boolean = ty match
    case TSet(ty) => true
    case _ => super.isTypeSupported(ty)

  private var setCompCollRelations: Set[Relation] = _
  private var setMembershipRelations: Set[Relation] = _
  private var setCompAggCounter: Int = _
  private var currentModule: Module = _

  private def createSetAggOp(name: Name, elemTy: ScalaType): ScalaMonoAggregationOperator =
    val ty = elemTy.name
    ScalaMonoAggregationOperator(name, ScalaType(s"$ty"), ScalaType(s"Set[$ty]"), initCode = s"Set[$ty]()", addCode = s"(st: Set[$ty], a: $ty) => st + a")

  override def visitModule(module: Module): Module =
    setCompAggCounter = 0
    setCompCollRelations = Set()
    setMembershipRelations = Set()
    currentModule = module
    val mod = super.visitModule(module)
    mod.copy(contents = mod.contents ++ setCompCollRelations ++ setMembershipRelations)

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case SetLit(ts) =>
      val setTy = term.typ.get.ty
      val elemTy = setTy.asInstanceOf[TSet].ty
      val params = ts.zipWithIndex map {case (t, i) => s"v$i: ${ScalaInca.compileType(elemTy).name}"}
      val lits = ts.indices map (i => s"v$i")
      Seq(ScalaTerm(
        s"(${params.mkString(", ")}) => Set(${lits mkString ", "})",
        ScalaInca.compileType(setTy),
        ts flatMap visitTerm 
      ))
    case SetUnion(t1, t2) =>
      val st1 = visitTerm(t1).head
      val st2 = visitTerm(t2).head
      val setTy = ScalaInca.compileType(t1.typ.get.ty).name
      Seq(ScalaTerm(s"(s1: $setTy, s2: $setTy) => s1 | s2", ScalaType(setTy), Seq(st1, st2)))
    case SetIntersection(t1, t2) =>
      val st1 = visitTerm(t1).head
      val st2 = visitTerm(t2).head
      val setTy = ScalaInca.compileType(t1.typ.get.ty).name
      Seq(ScalaTerm(s"(s1: $setTy, s2: $setTy) => s1 & s2", ScalaType(setTy), Seq(st1, st2)))
    case SetComprehension(elem, atoms) =>
      val elemITy = elem.typ.get.ty // inca type
      val elemSTy = ScalaInca.compileType(elemITy) // scala type
      val collNm = gensym.freshName("set$comp$elem")
      // collect all free variables
      val vars = term.vars
      vars.foreach(v => v.typ.getOrElse(throw new IllegalStateException(s"Set lowering requires types IR in $v")))
      val (boundVars, bindingVars) = vars.partition(!_.typ.get.mode.isBinding)
      val freeVars = boundVars.toSet diff bindingVars.toSet
      val inputParams = freeVars.toSeq.map(v => v.name -> TDemand(ScalaInca.compileType(v.typ.get.ty)))
      val ats = atoms.flatMap(visitAtom)
      val Seq(visitedElem) = visitTerm(elem)
      setCompAggCounter += 1
      setCompCollRelations += Relation(s"coll$$set$$comp$$$setCompAggCounter", inputParams.map((nm, ty) => Param(nm, ty)) :+ Param(collNm, elemSTy), Seq(Body(ats :+ Eq(Var(collNm), visitedElem))))
      val aggOp = createSetAggOp(s"AggOp$$Set$$${elemSTy.name}", elemSTy)
      val resNm = gensym.freshName("set$comp$res")
      val aggAtom = ScalaAggregationAtom(aggOp, s"coll$$set$$comp$$$setCompAggCounter", Var(resNm), inputParams.map((nm, _) => Var(nm)) :+ Var(gensym.freshName("_")), inputParams.size)
      val callAtom = Call(s"coll$$set$$comp$$$setCompAggCounter", inputParams.map((nm, _) => Var(nm).arg) :+ Var(gensym.freshName("anything")).arg)
      Seq(Block(Seq(callAtom, aggAtom), Var(resNm)))
    case SetFrom(name) if currentModule.relations.getOrElse(name.name, throw new IllegalStateException(s"Unknown relation $name")).params.size == 1 =>
      val rel = currentModule.relations(name.name)
      val elemITy = rel.params.head.ty
      val elemSTy = ScalaInca.compileType(elemITy)
      val collNm = gensym.freshName("set$from$elem")
      val collRel = Relation(s"coll$$set$$from$$$setCompAggCounter", Seq(Param(collNm, elemITy)), Seq(Body(Seq(Call(name, Seq(Var(collNm).arg))))))
      val aggOp = createSetAggOp(s"AggOp$$Set$$${elemSTy.name}", elemSTy)
      val resNm = gensym.freshName("set$from$res")
      val aggAtom = ScalaAggregationAtom(aggOp, s"coll$$set$$from$$$setCompAggCounter", Var(resNm), Seq(Var(gensym.freshName("_"))), 0)
      setCompAggCounter += 1
      setCompCollRelations += collRel
      Seq(Block(Seq(aggAtom), Var(resNm)))
    case SetFrom(name) if currentModule.relations.getOrElse(name.name, throw new IllegalStateException(s"Unknown relation $name")).params.size > 1 =>
      val rel = currentModule.relations(name.name)
      val elemITy = TTuple(rel.params map (p => p.ty))
      val elemSTy = ScalaInca.compileType(elemITy)
      val collVarNm = rel.params.map(_ => gensym.freshName("set$from$elem"))
      val collTupleNm = gensym.freshName("set$from$elem$tuple")
      val collRel = Relation(
        s"coll$$set$$from$$$setCompAggCounter",
        Seq(Param(collTupleNm, elemSTy)),
        Seq(Body(Seq(
          Call(name, collVarNm.map(nm => Var(nm).arg)),
          Eq(
            Var(collTupleNm),
            ScalaTerm(
              s"(${rel.params.zipWithIndex.map {case (Param(_, pty), i) => s"v$i: ${pty.asInstanceOf[ScalaType].name}"}.mkString(", ")}) => (${rel.params.indices.map(i => s"v$i").mkString(", ")})",
              elemSTy,
              collVarNm.map(nm => Var(nm))
            )
          )
        ))))
      val aggOp = createSetAggOp(s"AggOp$$Set$$${elemSTy.name}", elemSTy)
      val resNm = gensym.freshName("set$from$res")
      val aggAtom = ScalaAggregationAtom(aggOp, s"coll$$set$$from$$$setCompAggCounter", Var(resNm), Seq(Var(gensym.freshName("_"))), 0)
      setCompAggCounter += 1
      setCompCollRelations += collRel
      Seq(Block(Seq(aggAtom), Var(resNm)))
    case _ => super.visitTerm(term)
  }

  private def createRelName(name: String): Name =
    val invalidSym = Seq("[", "]")
    invalidSym.foldLeft(name)(_.replace(_, "$"))


  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) { atom match
    case SetMember(mem, s) =>
      // create a relation that enumerates all items in the set
      // (applying set contains method at here is not appropriate because
      // `i` is an unbound variable in the semantics of atom SetMember(i, s).
      val memTy = ScalaInca.compileType(mem.typ.get.ty).name
      val setTy = ScalaInca.compileType(s.typ.get.ty).name
      val memRelName = createRelName(s"Set$$Mem$$$memTy")
      val memRel = Relation(memRelName, Seq(Param("elem", ScalaType(memTy)), Param("s", TDemand(ScalaType(setTy)))), Seq(
        Body(Seq(
          Eq(ScalaTerm(s"(s: $setTy) => s.nonEmpty", ScalaType.bool, Seq(Var("s"))), TRUE),
          Eq(ScalaTerm(s"(s: $setTy) => s.head", ScalaType(memTy), Seq(Var("s"))), Var("elem"))
        )),
        Body(Seq(
          Eq(ScalaTerm(s"(s: $setTy) => s.nonEmpty", ScalaType.bool, Seq(Var("s"))), TRUE),
          Call(memRelName, Seq(Var("elem").arg, ScalaTerm(s"(s: $setTy) => s.tail", ScalaType(setTy), Seq(Var("s"))).arg))
        ))
      ))
      setMembershipRelations += memRel
      Seq(Call(memRelName, Seq(visitTerm(mem).head.arg, visitTerm(s).head.arg)))
    case _ => super.visitAtom(atom)
  }

  override def visitAggregationOperator(op: AggregationOperator): AggregationOperator = op match
    case MonoAggregationOperator(NaiveSetMonoDefinition(TSet(ty))) => ???
    case _ => super.visitAggregationOperator(op)

