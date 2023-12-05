package inca.ir.extension.set

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, Deconstruct, TData}
import inca.ir.extension.demand.TDemand
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.tuple.TupleLit
import inca.ir.lowering.BaseLowering
import inca.util.namify

/*
 * Proposal: Represent set with IDs expressed as ADTs
 * E.g
 * main(z: Set[Int]) :- y == Set(1,2,3), somCall(y, z).
 * someCall(y: Set[Int], z: Set[Int]) :- z == (y U Set(2,4))
 *
 * Lowering:
 * SetADT_Int = Set$empty | Set$0 | Set$1 | Set$2(y)
 * SetADT_String = Set$S$0 | Set$S$1 | Set$S$2(y)
 *
 * set$Any(s: SetADT, x: Any) :- ...
 * set$Int(s: SetADT, x: Int) :- demand(s), match(s) {
 *   case Set$0 => (x == 1 v x == 2 v x == 3)
 *   case Set$1 => (x == 2 v x == 4)
 *   case Set$2(y) => (set(y, x) v set(Set$1, x))
 * }
 * set$String(s: SetADT_String, x: String) :- ...

 * set(s: SetADT, x: Int) :- demand(s), ?Set$0(s), (x == 1 v x == 2 v x == 3)
 * set(s: SetADT, x: Int) :- demand(s), ?Set$1(s), (x == 2 v x == 4)
 *
 * set(s: SetADT, x: Int) :- demand(s), ?Set$2(s, y), (set(y, x) v set(Set$1, x))
 *
 * main(z: Int) :- y == Set$0, somCall(y, z).
 * someCall(y: SetADT, z: Int) :- set(Set$2(y), z).
 *
 * We replace a Set with an ID that we represent by an ADT. Instead of passing around
 * a set, we pass around an ADT value. The ADT case must include all variables bound
 * in the Set expression. When we read a set, we query this set relation with the ID
 * we generated for the set.
 */
trait Lowering extends BaseLowering:

  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(block.IR, data.IR, demand.IR, disjunction.IR, tuple.IR)


  private trait SetEnum:
    def apply(elemVar: Name): Seq[Atom]

  private case class SetConstructor(name: Name, vars: Seq[(Name, Type)], setEnum: SetEnum)

  private var constructorCount: Map[Type, Int] = Map().withDefaultValue(0)
  private var constructors: Map[(Type, Term), SetConstructor] = Map()
  private def addConstructor(originalTerm: Term, setEnum: SetEnum): (Name, Seq[(Name, Type)]) =
    val memTy = visitType(memberType(originalTerm))
    constructors.get((memTy, originalTerm)) match
      case Some(SetConstructor(name, vars, _)) => (name, vars)
      case None =>
        val count = constructorCount(memTy)
        constructorCount += memTy -> (count + 1)
        val name = constructorNameOf(memTy, count)
        val vars = originalTerm.vars
        vars.foreach(v => v.typ.getOrElse(throw new IllegalStateException(s"Set lowering requires types IR in $v")))
        val (boundVars, bindingVars) = vars.partition(!_.typ.get.mode.isBinding)
        val freeVars = boundVars.toSet diff bindingVars.toSet
        val constructorParams = freeVars.toSeq.map(v => v.name -> visitType(v.typ.get.ty))
        constructors += (memTy, originalTerm) -> SetConstructor(name, constructorParams, setEnum)
        (name, constructorParams)
  private def callAddConstructor(originalTerm: Term, setEnum: SetEnum): Construct =
    val (name, vars) = addConstructor(originalTerm, setEnum)
    val cons = Construct(name, vars.map(v => Var(v._1)))
//    cons.typed(TSet(memTy).closed)
    cons

  private def dataNameOf(memTy: Type): Name = Name(s"Set$$${namify(memTy.toString)}$$")
  private def constructorNameOf(memTy: Type, count: Int) = Name(s"${dataNameOf(memTy)}$$$count")
  private def relNameOf(memTy: Type): Name = Name(s"${dataNameOf(memTy)}$$enum")

  private def makeSetDefinitions: Seq[ModuleEntry] =
    val types = constructors.groupBy(_._1._1).toSeq
    types.flatMap { case (memTy, terms) =>
      val (data, rel) = defunctionalizeSet(memTy, terms.values.toSeq)
      Seq(data, rel)
    }

  /** Generates defunctionalize set data type and enumerating relation */
  private def defunctionalizeSet(memTy: Type, constructors: Seq[SetConstructor]): (DataDefinition, Relation) =
    val dataName = dataNameOf(memTy)
    val relName = relNameOf(memTy)
    val setParam = Param("$set", TDemand(TData(dataName)))
    val elemParam = Param("$elem", memTy)

    val (cases, rules) = constructors.map { case SetConstructor(consName, caseVars, setEnum) =>
      val caseDef = CaseDefinition(consName, caseVars.map(_._2))

      val atoms = setEnum(elemParam.name)
      if (atoms.isEmpty) {
        (caseDef, None)
      } else {
        val rule = Body(Deconstruct(Var(setParam.name), consName, caseVars.map(v => Var(v._1).arg)) +: atoms)
        (caseDef, Some(rule))
      }
    }.unzip

    val data = DataDefinition(dataName, cases)
    val rel = Relation(relName, Seq(setParam, elemParam), rules.flatten)
    (data, rel)

  private var currentModule: Module = _
  protected override def visitModule(module: Module): Module =
    currentModule = module
    constructors = Map()
    val m = super.visitModule(module)
    val defs = makeSetDefinitions
    m.copy(contents = m.contents ++ defs)

  private def memberType(t: Term): Type = t.typ.getOrElse(throw new IllegalStateException(s"Set lowering requires typed IR, type missing in $t")).ty match
    case TSet(memTy) => memTy
    case ty => throw new IllegalStateException(s"Expected set type for $t but it has type $ty")

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case TSet(memTy) => TData(dataNameOf(memTy))
      case _ => super.visitType(ty)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case SetLit(ts) =>
      val elems = ts.map(visitTerm)
      val setEnum = new SetEnum:
        override def apply(elemVar: Name): Seq[Atom] =
          if (elems.isEmpty)
            Seq()
          else
            Seq(Disjunction(elems.map(ts => DisjunctionAlternative(ts.map(Eq(Var(elemVar), _))))))
      Seq(callAddConstructor(term, setEnum))
    case SetFrom(name) =>
      val rel = currentModule.relations.getOrElse(name, throw new IllegalStateException(s"Unknown relation $name"))
      val setEnum = new SetEnum:
        override def apply(elemVar: Name): Seq[Atom] =
          val args = rel.params.map(p => Var(gensym.freshName(p.name)))
          Seq(Call(name, args.map(_.arg)), Eq(TupleLit.make(args), Var(elemVar)))
      Seq(callAddConstructor(term, setEnum))
    case SetUnion(t1, t2) =>
      val Seq(s1) = visitTerm(t1)
      val memTy1 = memberType(t1)
      val Seq(s2) = visitTerm(t2)
      val memTy2 = memberType(t2)
      val setEnum = new SetEnum:
        override def apply(elemVar: Name): Seq[Atom] = Seq(
          Disjunction(Seq(
            DisjunctionAlternative(Call(relNameOf(memTy1), Seq(s1.arg, Var(elemVar).arg))),
            DisjunctionAlternative(Call(relNameOf(memTy2), Seq(s2.arg, Var(elemVar).arg)))
          ))
        )
      Seq(callAddConstructor(term, setEnum))
    case SetIntersection(t1, t2) =>
      val Seq(s1) = visitTerm(t1)
      val memTy1 = memberType(t1)
      val Seq(s2) = visitTerm(t2)
      val memTy2 = memberType(t2)
      val setEnum = new SetEnum:
        override def apply(elemVar: Name): Seq[Atom] = Seq(
          Call(relNameOf(memTy1), Seq(s1.arg, Var(elemVar).arg)),
          Call(relNameOf(memTy2), Seq(s2.arg, Var(elemVar).arg))
        )
      Seq(callAddConstructor(term, setEnum))
    case SetComprehension(build, atoms) =>
      val ats = atoms.flatMap(visitAtom)
      val ts = visitTerm(build)
      val setEnum = new SetEnum:
        override def apply(elemVar: Name): Seq[Atom] =
          ats :+ Eq(TupleLit.make(ts), Var(elemVar))
      Seq(callAddConstructor(term, setEnum))
    case _ => super.visitTerm(term)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case SetMember(elemTerm, setTerm) => preserveHints(atom) {
      val Seq(s) = visitTerm(setTerm)
      val memTy = memberType(setTerm)
      val ts = visitTerm(elemTerm)
      ts.map(elem => Call(relNameOf(memTy), Seq(s.arg, elem.arg)))
    }
    case _ => super.visitAtom(atom)

/**

 def test(): Set[Any] =
   {1,2,3}

*/