package inca.ir.valueNumbering

import inca.ir
import inca.ir.*
import inca.ir.typing.IRTypechecker

import scala.collection.mutable
import inca.ir.visitors.IRVisitor

import scala.annotation.tailrec


/** for value numbering constructs from BaseIR */
trait BaseValueNumbering extends IRVisitor {
  // config
  def normalizeDoubles: Boolean = false
  def useDefiningTerm: Boolean = false
  def useFixPointIteration: Boolean = true
  def printVNResults: Boolean = true

  protected case class CongruenceClass(valueId: ValueId, var leader: Term, var definingTerm: Term) {
    override def toString: String =
      s"Congruence Class: Id = $valueId, leader = $leader, definingTerm = $definingTerm"

    def changeLeaderIfNecessary(t: Term): Unit = { // also prevents type errors since in second pass otherwise might propagate unbound Vars
      if (isConst(t)) {
        if (isConst(leader) && leader != t) throw new IllegalStateException(s"Term $t can not equal $leader with valueId $valueId")
        leader = t
      }
      if (!isParam(leader) && !isConst(leader) && isParam(t))
        leader = t
    }

    def changeDefTermIfNecessary(t: Term, updateDefTermIfNecessary: Boolean = false): Unit = {
      if (isConst(t)) definingTerm = t
      else if (updateDefTermIfNecessary && definingTerm.isInstanceOf[Var] && !t.isInstanceOf[Var]) // resembles case that CongruenceClass was initially created for Var bound in Call
        definingTerm = t
    }

    def updateCongrClassIfNecessary(t: Term, updateDefTermIfNecessary: Boolean = false): Unit = {
      changeLeaderIfNecessary(t)
      if (updateDefTermIfNecessary)
        changeDefTermIfNecessary(t)
    }

  }

  protected var congrClasses: mutable.Map[ValueId, CongruenceClass] = mutable.Map()
  protected var valueNumbers: ValueIds[Term] = new ValueIds()

  protected def getCongrClassOf(t: Term): CongruenceClass = congrClasses(valueNumbers(t))

  protected def getReplacementTerm(t: Term): Term = {
    if (!congrClasses.contains(valueNumbers(t))) return t

    val leader = getCongrClassOf(t).leader
      leader match {
        case vari@Var(_) => newVar(vari.name,t.typ)
        case _ => leader
      }
  }

  protected def getDefiningTerm(t: Term): Term = {
    if (valueNumbers.contains(t)){
      if (congrClasses.contains(valueNumbers(t))) {
        return getCongrClassOf(t).definingTerm
      }
    }
    return t
  }

  private def updateValueNumbersAndCongrClasses(term: Term, toId: ValueId): Unit = {
    val fromId = getIdOf(term)
    if (fromId != toId) updateValueNumbersAndCongrClasses(fromId, toId)
    else if (congrClasses.contains(toId)) {
      congrClasses(toId).updateCongrClassIfNecessary(term)
    }
  }

  private def updateValueNumbersAndCongrClasses(fromId: ValueId, toId: ValueId): Unit = {
    var updateToCongrClass = false
    if (congrClasses.contains(fromId) && congrClasses.contains(toId)) {
      congrClasses(toId).changeLeaderIfNecessary(congrClasses(fromId).leader)
      congrClasses(toId).changeDefTermIfNecessary(congrClasses(fromId).definingTerm)
    }
    else if (congrClasses.contains(fromId) && !congrClasses.contains(toId)) {
      congrClasses.update(toId, CongruenceClass(toId, congrClasses(fromId).leader, congrClasses(fromId).definingTerm))
      valueNumbers.getAllWithId(toId).foreach(t => congrClasses(toId).updateCongrClassIfNecessary(t))
    }
    else if (!congrClasses.contains(fromId) && congrClasses.contains(toId)) {
      updateToCongrClass = true
    }
    valueNumbers.getAllWithId(fromId).foreach(t =>
      valueNumbers.update(t,toId)
      if (updateToCongrClass) {
        congrClasses(toId).updateCongrClassIfNecessary(t)
      }
    )
    congrClasses.remove(fromId)
  }


  private enum Phase:
    case initial
    case repetition
    case removeInValidBodies
  private var phase: Phase = _


  type RelationName = Name
  type BodyIndex = Int
  private var currentRelationName: RelationName = _
  private var currentBodyIndex: BodyIndex = -1

  def printResults(): Unit = {
    if !printVNResults then return
    println(s"Results from Relation $currentRelationName body $currentBodyIndex")
    println(s"Congruence Classes Info:")
    println("\t" + congrClasses.mkString("\n\t") + "\n")
    valueNumbers.printResults()
  }

  var analysisResults: Map[(RelationName,BodyIndex), (ValueIds[Term], mutable.Map[ValueId, CongruenceClass])] = Map()
  
  var isValidBody: Map[(RelationName, BodyIndex), Boolean] = Map()

  
  def valueNumbering(module: ir.Module): ir.Module = visitModule(module)

  override def visitModule(module: Module): Module = {
    if printVNResults then println(s"before VN: \n$module\n")

    phase = Phase.initial // in initial phase congrClass is empty -> it can be assumed that all saved Vars are bound
    val tempResult = super.visitModule(module)

    phase = Phase.repetition
    val result = repetitionPhase(tempResult)

    phase = Phase.removeInValidBodies
    val finalResult = removeInvalidBodies(result)

    if printVNResults then println(s"after VN: \n$finalResult")
    finalResult
  }

  @tailrec
  private def repetitionPhase(module: Module): Module = {
    val result = super.visitModule(module)
    val typechecker = new IRTypechecker{}
    typechecker.checkProgram(Seq(result))
    if (result != module && useFixPointIteration){
      return repetitionPhase(result)
    }
    else {
      return result
    }
  }

  private def removeInvalidBodies(module: Module): Module = {
    super.visitModule(module)
  }


  protected def getIdOf(t: Term): ValueId = valueNumbers.getIdOf(t)

  protected def normalize(term: Term): Term = term

  protected def isConst(term: Term): Boolean = term match {
    case Cast(t, ty) => isConst(t)
    case _ => false
  }


  private def newVar(name: Name, ty: Option[TermType] = None): Var = {
    val v = Var(RefByName(name))
    v.typ = ty
    v
  }

  private var relationParams: Seq[Name] = Seq()
  protected def isParam(t: Term): Boolean = t match {
    case vari@Var(_) => relationParams.contains(vari.name)
    case _ => false
  }


  override def visitRelation(relation: Relation): Seq[Relation] = {
    currentRelationName = relation.name
    relationParams = relation.params.map(_.name)
    currentBodyIndex = -1
    super.visitRelation(relation)
  }


  private var validBody: Boolean = _

  override def visitBody(body: Body): Seq[Body] = {
    currentBodyIndex += 1

    if (phase == Phase.removeInValidBodies){
      if (!isValidBody((currentRelationName, currentBodyIndex)))
        return Seq()
      else
        return Seq(body)
    }

    if (phase == Phase.repetition){
      if (!isValidBody((currentRelationName, currentBodyIndex))) return Seq(body)

      congrClasses = analysisResults((currentRelationName, currentBodyIndex))._2
      valueNumbers = analysisResults((currentRelationName, currentBodyIndex))._1
    }

    validBody = true // body is invalid if found to contain Eq(lhs,rhs) with lhs and rhs constant and lhs != rhs

    val newBody = super.visitBody(body)

    printResults()

    // reset congrClasses (otherwise not known when variables are unbound)
    analysisResults = analysisResults + ((currentRelationName,currentBodyIndex) -> (valueNumbers, congrClasses))
    isValidBody = isValidBody + ((currentRelationName,currentBodyIndex) -> validBody)

    congrClasses = mutable.Map[ValueId, CongruenceClass]()
    valueNumbers = new ValueIds[Term]()

    return newBody
  }


  override def visitAtom(atom: Atom): Seq[Atom] = atom match {
    case Eq(vari@Var(_), e, false) if vari.mode.isBinding =>
      // in case a redundant binding is found it will be removed unless it belongs to a parameter
      valueNumberVar(vari, e, dontRemove = isParam(vari))
    case Eq(e, vari@Var(_), false) if vari.mode.isBinding =>
      valueNumberVar(vari, e, dontRemove = isParam(vari))
    case Eq(vari@Var(_), e, false) =>
      // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality
      valueNumberVar(vari, e, dontRemove = true)
    case Eq(e, vari@Var(RefByName(Name(_))), false) =>
      valueNumberVar(vari, e, dontRemove = true)

    case call@Call(_, args, false) =>  treatBindingsInCall(call, args)

    case call@ExtensionalCall(_, args, false) => treatBindingsInCall(call, args)

    case _ => super.visitAtom(atom)
  }


  /** replaces term with Var if possible */
  override def visitTerm(term: Term): Seq[Term] = {
    if isConst(term) then return Seq(term) // dont replace constant terms and no need to normalize them
    if (congrClasses.contains(valueNumbers(term)) && isAllowedToReplace(term)) then return Seq( getReplacementTerm(term) )

    val newTerm = super.visitTerm(term).head
    newTerm.typ = term.typ

    val newTermId: ValueId = valueNumbers(newTerm)
    val termId: ValueId = valueNumbers(term)

    if (newTermId != termId){
      // ids not equal but terms are equal because newTerm was obtained by rewriting term -> should have same id
      updateValueNumbersAndCongrClasses(termId, newTermId)
    }

    val normalizedTerm = normalize(newTerm)
    if (!valueNumbers.contains(normalizedTerm)){ // normalizedTerm not seen before
      valueNumbers.update(normalizedTerm, newTermId)
      if (congrClasses.contains(newTermId)) {
        congrClasses(newTermId).updateCongrClassIfNecessary(normalizedTerm)
      }
    }
    else {
      // normalized term already has an id -> update term and newTerm to that id
      val normId = valueNumbers(normalizedTerm)
      if (congrClasses.contains(newTermId)) {
        congrClasses(newTermId).updateCongrClassIfNecessary(normalizedTerm)
      }
      if (newTermId != normId) updateValueNumbersAndCongrClasses(newTermId, normId)

      if (congrClasses.contains(normId) && isAllowedToReplace(newTerm)) {
          return Seq(getReplacementTerm(normalizedTerm))
        }
    }

      return Seq(normalizedTerm)
  }


  // prevent terms that contain Vars with unknown value from being replaced (while not preventing Vars from being replaced)
  protected def isAllowedToReplace(term: Term): Boolean = term.vars.isEmpty || term.isInstanceOf[Var]


  private def valueNumberVar(vari: Var, t: Term, dontRemove: Boolean = false): Seq[Eq] = {
    val newTerm = if isParam(t) then t else visitTerm(t).head
    val newVari = if isParam(vari) then vari else visitTerm(vari).head

    // prevent learning from unsatisfiable Eq constraints and leave them in the body -> remove body later
    if (isConst(getReplacementTerm(newTerm)) && isConst(getReplacementTerm(newVari)) && getReplacementTerm(newTerm) != getReplacementTerm(newVari)) {
      validBody = false
      return Seq(Eq(newVari,newTerm))
    }

    val termId: ValueId = getIdOf(newTerm)
    updateValueNumbersAndCongrClasses(newVari, termId)

    if (congrClasses.contains(termId)) {
      // remove "Assignment" or replace term
      if (dontRemove || phase == Phase.repetition) { // since only in 1st pass known that vari already computed/bound
        generateEqIfNecessary(newVari, newTerm)
      }
      else {
        Seq()
      }
    }

    else {
      // if term is a constant then use it as leader of its congruence class
      if (isConst(newTerm)) {
        congrClasses.update(termId, CongruenceClass(termId, newTerm, newTerm))
        if !dontRemove then return Seq() // remove binding of constant -> usages of var are replaced with constant
      }
      else {
        congrClasses.update(termId, CongruenceClass(termId, newVari, newTerm))
        congrClasses(termId).updateCongrClassIfNecessary(newTerm, updateDefTermIfNecessary = true)
      }

      generateEqIfNecessary(newVari, newTerm)
    }
  }

  private def generateEqIfNecessary(lhs: Term, rhs: Term): Seq[Eq] = {
    if (lhs == rhs) {
      Seq()
    }
    else
    Seq(Eq(lhs, rhs))
  }


  private def treatBindingsInCall(call: Call | ExtensionalCall, args: Seq[Arg]): Seq[Atom] = {
    val newArgs: Seq[Arg] = args.flatMap(visitArg)
    val newCall = call match{
      case Call(ref,_,neg) => Call(ref,newArgs,neg)
      case ExtensionalCall(ref,_,neg) => ExtensionalCall(ref,newArgs,neg)
    }
    Seq(newCall)
  }


  override def visitArg(arg: Arg): Seq[Arg] = arg match {
    case TermArg(vari@Var(_)) if vari.mode.isBinding =>   // add binding vars to maps
      Seq(TermArg(conservativeBinding(vari)))
    case _ => super.visitArg(arg)
  }

  protected def conservativeBinding(vari: Var): Term = { // conservative assumption that not equal to any known terms
    val newVari = if isParam(vari) then vari else visitTerm(vari).head
    val id = valueNumbers.getIdOf(newVari)
    // in the 1st pass: binding var becomes leader of its new congr class;
    // in 2nd pass: vari was replaced with leader -> newVari that was leader becomes new leader
    congrClasses.update(id, CongruenceClass(id, newVari, newVari))
    newVari
  }



}
