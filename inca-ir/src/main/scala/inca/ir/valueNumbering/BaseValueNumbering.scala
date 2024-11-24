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

  protected case class CongrClass(valueId: ValueId, var leader: Term, var definingTerm: Term) extends CongruenceClass {
    override def toString: String =
      s"Congruence Class: Id = $valueId, leader = $leader, definingTerm = $definingTerm"

    def changeLeaderIfNecessary(t: Term): Unit = { // also prevents type errors since in second pass otherwise might propagate unbound Vars
      if (isConst(t)) {
        if (isConst(leader) && leader != t) {
          println("#############################################################################")
          printResults()
          throw new IllegalStateException(s"ValueNumbering: Term $t cannot equal $leader with valueId $valueId while analyzing relation $currentRelationName body with index $currentBodyIndex")
        }
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
      congrClasses.update(toId, CongrClass(toId, congrClasses(fromId).leader, congrClasses(fromId).definingTerm))
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

  private var phase: Phase = _


  private type RelationName = Name
  private type BodyIndex = Int
  private var currentRelationName: RelationName = _
  private var currentBodyIndex: BodyIndex = -1

  def printResults(): Unit = {
    if (!printVNResults) return
    println(s"Results from Relation $currentRelationName body $currentBodyIndex")
    println(s"Congruence Classes Info:")
    println("\t" + congrClasses.mkString("\n\t") + "\n")
    valueNumbers.printResults()
  }


  private type ParamName = Name
  private var paramLeaders: Map[(RelationName, ParamName), Term] = Map() // TODO refactor ?

  private var inputRelations: Map[String,Relation] = _  // used to access analysis results of params of other relations than the currently processed one


  def valueNumbering(module: ir.Module): ir.Module = visitModule(module)

  override def visitModule(module: Module): Module = {
    if (printVNResults) println(s"before VN: \n$module\n")
    inputRelations = module.relations

    phase = Phase.initial // in initial phase congrClass is empty -> it can be assumed that all saved Vars are bound
    val tempResult = super.visitModule(module)

    phase = Phase.repetition // repeat with previous analysis results and rewritten bodies
    val result = repetitionPhase(tempResult)

    if (printVNResults) println(s"after VN: \n$result")
    result
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

  private var currentRelationParams: Seq[Name] = Seq()
  protected def isParam(t: Term): Boolean = t match {
    case vari@Var(_) => currentRelationParams.contains(vari.name)
    case _ => false
  }



  private def joinParams(relation: Relation): Unit = { // TODO refactor ?
    import ConstLattice.*

    val relName = relation.name
    val params = relation.params

    val leaders = params.map{ param =>
      val bodyLeaders = relation.bodies.map { body =>
        val vn = body.VNs(Var(param.name))
        val leader = body.congruenceClasses(vn).leader
        toConst(leader, isConst)
      }
      val leader = bodyLeaders.foldRight(Bot) { (elem, tempRes) => elem.join(tempRes) }
      param -> leader
    }.filter(_._2.isConst).toMap

    paramLeaders = paramLeaders ++ leaders.map { case (param, Const(t)) =>
      (relName, param.name) -> t
    }
  }


  override def visitRelation(relation: Relation): Seq[Relation] = {
    currentRelationName = relation.name
    currentRelationParams = relation.params.map(_.name)
    currentBodyIndex = -1
    val result = super.visitRelation(relation)

//    joinParams(result.head)

    result
  }


  private var validBody: Boolean = _

  override def visitBody(body: Body): Seq[Body] = {
    currentBodyIndex += 1

    if (phase == Phase.repetition){
      congrClasses = body.congruenceClasses
      valueNumbers = body.VNs
    }

    validBody = true // body is invalid if found to contain Eq(lhs,rhs) with lhs and rhs constant and lhs != rhs

    val newBody = super.visitBody(body).head

    if (!validBody) {
      congrClasses = mutable.Map[ValueId, CongruenceClass]()
      valueNumbers = new ValueIds[Term]()
      return Seq()
    }

    printResults()

    // remember analysis results in body
    newBody.VNs = valueNumbers
    newBody.congruenceClasses = congrClasses

    // reset congrClasses (otherwise not known when variables are unbound)
    congrClasses = mutable.Map[ValueId, CongruenceClass]()
    valueNumbers = new ValueIds[Term]()

    return Seq(newBody)
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

    case call@Call(_, _, false) => treatBindingsInCall(call)

    case call@ExtensionalCall(_, _, false) => treatBindingsInExtensionalCall(call)

    case _ => super.visitAtom(atom)
  }


  /** replaces term with Var if possible */
  override def visitTerm(term: Term): Seq[Term] = {
    if (isConst(term)) return Seq(term) // dont replace constant terms and no need to normalize them
    if (congrClasses.contains(valueNumbers(term)) && isAllowedToReplace(term)) return Seq( getReplacementTerm(term) )

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
    val newTerm = if (isParam(t)) t else visitTerm(t).head
    val newVari = if (isParam(vari))vari else visitTerm(vari).head

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
        congrClasses.update(termId, CongrClass(termId, newTerm, newTerm))
        if (!dontRemove) return Seq() // remove binding of constant -> usages of var are replaced with constant
      }
      else {
        congrClasses.update(termId, CongrClass(termId, newVari, newTerm))
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


  private def treatBindingsInCall(call: Call): Seq[Atom] = {

    /** helper method for [[treatBindingsInCall]] */
    def treatBinding(vari: Var, paramIndex: Int, refName: Name): Term = { // TODO refactor ?
      val paramName = inputRelations(refName).params(paramIndex).name
      val key = (refName, paramName)

      if (paramLeaders.contains(key)) {
//        if (paramLeaders(key).nonEmpty) {
          val leader = paramLeaders(key)//.get
          val id = getIdOf(leader)

          if (congrClasses.contains(id)) {
            congrClasses(id).updateCongrClassIfNecessary(leader)
          }
          else {
            congrClasses.update(id, CongrClass(id, leader, leader))
          }

          updateValueNumbersAndCongrClasses(vari, id)
          if (!isParam(vari)) return leader
          else return vari
//        }
      }

      return conservativeBinding(vari)
    }

    val Call(ref, args, neg) = call
    val newArgs: Seq[Arg] = args.zipWithIndex.map {
      case (TermArg(vari@Var(_)), i) if vari.mode.isBinding =>
        val newArg = treatBinding(vari, i, ref.name)
        TermArg(newArg)
      case (arg,_) => visitArg(arg).head
    }
    return Seq(Call(ref, newArgs, neg))
  }



  private def treatBindingsInExtensionalCall(call: ExtensionalCall): Seq[Atom] = {
    val ExtensionalCall(ref, args, neg) = call
    val newArgs: Seq[Arg] = args.flatMap(visitArg)
    Seq(ExtensionalCall(ref, newArgs, neg))
  }


  override def visitArg(arg: Arg): Seq[Arg] = arg match {
    case TermArg(vari@Var(_)) if vari.mode.isBinding =>   // add binding vars to maps
      Seq(TermArg(conservativeBinding(vari)))
    case _ => super.visitArg(arg)
  }

  protected def conservativeBinding(vari: Var): Term = { // conservative assumption that not equal to any known terms
    val newVari = if (isParam(vari)) vari else visitTerm(vari).head
    val id = valueNumbers.getIdOf(newVari)
    // in the 1st pass: binding var becomes leader of its new congr class;
    // in 2nd pass: vari was replaced with leader -> newVari that was leader becomes new leader
    congrClasses.update(id, CongrClass(id, newVari, newVari))
    newVari
  }



}
