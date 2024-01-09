package inca.ir.extension.map

import inca.ir.*
import inca.ir.Hint.preserveHints
import inca.ir.extension.*
import inca.ir.extension.block.Block
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, DataModuleEntry, Deconstruct, TData}
import inca.ir.extension.demand.{DemandIgnoreCallHint, TDemand}
import inca.ir.extension.disjunction.{Disjunction, DisjunctionAlternative}
import inca.ir.extension.tuple.TupleLit
import inca.ir.lowering.BaseLowering
import inca.util.namify

import scala.collection.immutable.{AbstractSeq, LinearSeq}


trait Lowering extends BaseLowering:
  override val name: String = "Map"
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(block.IR, data.IR, demand.IR, disjunction.IR, tuple.IR)


  private trait MapEnum:
    def apply(keyVar: Name, valVar: Name): Seq[Atom]

  private case class MapConstructor(name: Name, vars: Seq[(Name, Type)], mapEnum: MapEnum)

  private var constructorCount: Map[(Type, Type), Int] = Map().withDefaultValue(0)
  private var mapTypes: Set[TMap] = Set()
  private var mapConstructors: Map[((Type, Type), Term), MapConstructor] = Map()
  private def addConstructor(originalTerm: Term, mapEnum: MapEnum): (Name, Seq[(Name, Type)]) =
    val (keyTyPre, valTyPre) = keyValType(originalTerm)
    val keyTy = visitType(keyTyPre)
    val valTy = visitType(valTyPre)
    mapConstructors.get(((keyTy, valTy), originalTerm)) match
      case Some(MapConstructor(name, vars, _)) => (name, vars)
      case None =>
        val count = constructorCount((keyTy, valTy))
        constructorCount += (keyTy, valTy) -> (count + 1)
        val name = constructorNameOf(keyTy, valTy, count)
        val vars = originalTerm.vars
        vars.foreach(v => v.typ.getOrElse(throw new IllegalStateException(s"Set lowering requires types IR in $v")))
        val (boundVars, bindingVars) = vars.partition(!_.typ.get.mode.isBinding)
        val freeVars = boundVars.toSet diff bindingVars.toSet
        val constructorParams = freeVars.toSeq.map(v => v.name -> visitType(v.typ.get.ty))
        mapConstructors += ((keyTy, valTy), originalTerm) -> MapConstructor(name, constructorParams, mapEnum)
        (name, constructorParams)
  private def callAddConstructor(originalTerm: Term, setEnum: MapEnum): Construct =
    val (name, vars) = addConstructor(originalTerm, setEnum)
    val cons = Construct(RefByName(name), vars.map(v => Var(v._1)))
    cons

  private def dataNameOf(keyTy: Type, valTy: Type): Name = Name(s"Map$$${namify(keyTy.toString)}_${namify(valTy.toString)}$$")
  private def constructorNameOf(keyTy: Type, valTy: Type, count: Int) = Name(s"${dataNameOf(keyTy, valTy)}$$$count")
  private def relNameOf(keyTyPre: Type, valTyPre: Type): Name = {
    val keyTy = visitType(keyTyPre)
    val valTy = visitType(valTyPre)
    val name1 = Name(s"${dataNameOf(keyTy, valTy)}$$rel")
    name1
  }

  private def makeMapDefinitions: Seq[ModuleEntry] =
    val defaulConstructorsByType = mapTypes.map(t => (t.k, t.v) -> Map()).toMap
    val constructorsByType = mapConstructors.groupBy(_._1._1)
    val types = defaulConstructorsByType ++ constructorsByType
    types.flatMap { case ((keyTy, valTy), terms) =>
      val (datas, rel) = defunctionalizeMap(keyTy, valTy, terms.values.toSeq)
      datas :+ rel
    }.toSeq

  /** Generates defunctionalize map data type and relation */
  private def defunctionalizeMap(keyTy: Type, valTy: Type, constructors: Seq[MapConstructor]): (Seq[DataModuleEntry], Relation) =
    val dataName = dataNameOf(keyTy, valTy)
    val relName = relNameOf(keyTy, valTy)
    val mapParam = Param("$map", TDemand(TData(dataName)))
    val keyParam = Param("$key", TDemand(keyTy))
    val valParam = Param("$val", valTy)

    val data = DataDefinition(dataName)
    val (cases, rules) = constructors.map { case MapConstructor(consName, caseVars, mapEnum) =>
      val caseDef = CaseDefinition(consName, caseVars.map(_._2), TData(dataName))

      val atoms = mapEnum(keyParam.name, valParam.name)
      if (atoms.isEmpty) {
        (caseDef, None)
      } else {
        val rule = Body(
          Deconstruct(Var(mapParam.name), RefByName(consName), caseVars.map(v => Var(v._1).arg), false)
            +: atoms)
        (caseDef, Some(rule))
      }
    }.unzip

    val rel = Relation(relName, Seq(mapParam, keyParam, valParam), rules.flatten)
    (data +: cases, rel)

  private var currentModule: Module = _
  override def visitModule(module: Module): Module =
    currentModule = module
    mapTypes = Set()
    mapConstructors = Map()
    val m = super.visitModule(module)
    val defs = makeMapDefinitions
    m.copy(contents = m.contents ++ defs)

  private def keyValType(t: Term): (Type,Type) = t.typ.getOrElse(throw new IllegalStateException(s"Map lowering requires typed IR, type missing in $t")).ty match
    case TMap(keyTy, valTy) => (keyTy, valTy)
    case ty => throw new IllegalStateException(s"Expected set type for $t but it has type $ty")

  override def visitType(ty: Type): Type = preserveHints(ty) {
    ty match
      case tm@TMap(keyTy, valTy) =>
        mapTypes += tm
        TData(dataNameOf(keyTy, valTy))
      case _ => super.visitType(ty)
  }

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term) { term match
    case MapLit(ts) =>
      val elems = ts.map(tt => visitTerm(tt._1).zip(visitTerm(tt._2)))
      val mapEnum = new MapEnum:
        override def apply(keyVar: Name, valVar: Name): Seq[Atom] =
          if (elems.isEmpty)
            Seq()
          else
            Seq(Disjunction(elems.map(ts => DisjunctionAlternative(ts.flatMap { tt =>
              Seq(Eq(Var(keyVar), tt._1), Eq(Var(valVar), tt._2))
            }))))
      Seq(callAddConstructor(term, mapEnum))
    case MapFrom(name) =>
      val rel = currentModule.relations.getOrElse(name, throw new IllegalStateException(s"Unknown relation $name"))
      val mapEnum = new MapEnum:
        override def apply(keyVar: Name, valVar: Name): Seq[Atom] =
          val params = rel.params.map(p => p -> Var(gensym.freshName(p.name)))
          val args = params.map(_._2)
          val (keyParams, valParams) = params.partition(_._1.ty.isInstanceOf[TDemand])
          val keyArgs = keyParams.map(_._2)
          val valArgs = valParams.map(_._2)
          Seq(
            Eq(TupleLit.make(keyArgs), Var(keyVar)),
            Call(name, args.map(_.arg)),
            Eq(TupleLit.make(valArgs), Var(valVar))
          )
      Seq(callAddConstructor(term, mapEnum))
    case MapFun(params, valTerm) =>
      val terms = visitTerm(valTerm)
      val mapEnum = new MapEnum:
        override def apply(keyVar: Name, valVar: Name): Seq[Atom] =
          val inputs = params.map(p => Var(p.name))
          Eq(TupleLit.make(inputs), Var(keyVar)) +:
          terms.map(Eq(_, Var(valVar)))
      Seq(callAddConstructor(term, mapEnum))
    case MapPlus(map, key, value) =>
      ???
    case MapUnion(t1, t2) =>
      val (keyTy1, valTy1) = keyValType(t1)
      val (keyTy2, valTy2) = keyValType(t2)
      val Seq(s1) = visitTerm(t1)
      val Seq(s2) = visitTerm(t2)
      val mapEnum = new MapEnum:
        override def apply(keyVar: Name, valVar: Name): Seq[Atom] = Seq(
          Disjunction(Seq(
            DisjunctionAlternative(Call(relNameOf(keyTy1, valTy1), Seq(s1.arg, Var(keyVar), Var(valVar).arg))),
            DisjunctionAlternative(Call(relNameOf(keyTy2, valTy2), Seq(s2.arg, Var(keyVar), Var(valVar).arg)))
          ))
        )
      Seq(callAddConstructor(term, mapEnum))
    case MapConcat(t1, t2) =>
      val (keyTy1, valTy1) = keyValType(t1)
      val (keyTy2, valTy2) = keyValType(t2)
      val Seq(s1) = visitTerm(t1)
      val Seq(s2) = visitTerm(t2)
      val mapEnum = new MapEnum:
        override def apply(keyVar: Name, valVar: Name): Seq[Atom] = Seq(
          Disjunction(Seq(
            DisjunctionAlternative(
              // only retain m1(k) = v if k not in m2
              Call(relNameOf(keyTy2, valTy2), Seq(s2.arg, Var(keyVar).arg, WildcardArg()), neg = true),
              Call(relNameOf(keyTy1, valTy1), Seq(s1.arg, Var(keyVar).arg, Var(valVar).arg))
            ),
            DisjunctionAlternative(
              Call(relNameOf(keyTy2, valTy2), Seq(s2.arg, Var(keyVar), Var(valVar).arg))
            )
          ))
        )
      Seq(callAddConstructor(term, mapEnum))
    case MapLookUp(map, key) =>
      val (keyTy, valTy) = keyValType(map)
      val Seq(m) = visitTerm(map)
      val valVar = Name(gensym.fresh("map$lookup"))
      val atoms = visitTerm(key).map { keyTerm =>
        Call(relNameOf(keyTy, valTy), Seq(m.arg, keyTerm.arg, Var(valVar).arg))
      }
      Seq(block.Block(atoms, Var(valVar)))
    case MapComprehension(key, value, atoms) =>
      val ats = atoms.flatMap(visitAtom)
      val ks = visitTerm(key)
      val vs = visitTerm(value)
      val mapEnum = new MapEnum:
        override def apply(keyVar: Name, valVar: Name): Seq[Atom] =
          ats :+ Disjunction(ks.zip(vs).map((k, v) =>
            DisjunctionAlternative(Seq(
              Eq(k, Var(keyVar)),
              Eq(v, Var(valVar))
            )))
          )
      Seq(callAddConstructor(term, mapEnum))
    case _ => super.visitTerm(term)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case MapContains(map, key) =>
      val (keyTy, valTy) = keyValType(map)
      val Seq(m) = visitTerm(map)
      val atoms = visitTerm(key).map { keyTerm =>
        Call(relNameOf(keyTy, valTy), Seq(m.arg, keyTerm.arg, WildcardArg())).addHint(DemandIgnoreCallHint)
      }
      atoms
    case _ => super.visitAtom(atom)
