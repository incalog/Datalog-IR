package inca.ir.valueNumbering

import inca.ir
import inca.ir.*
import inca.ir.typing.IRTypechecker
import inca.ir.valueNumbering.VNTables.{CongrClassesTable, CongruenceClassTerms, VNTablesRelations, VNTablesTerms, VNTablesTrait, ValueId, ValueIds}

import scala.collection.mutable
import inca.ir.visitors.IRVisitor

import scala.annotation.tailrec


/** for value numbering constructs from BaseIR */
trait BaseValueNumbering extends IRVisitor {
  // config
  def normalizeDoubles: Boolean = false
  def useDefiningTerm: Boolean = false
  def useFixPointIteration: Boolean = true
  def printVNResults: Boolean = false
  def printVNStatistics: Boolean = false

  protected case class CongrClass(valueId: ValueId, var leader: Term) extends CongruenceClassTerms {
    override val isConstTerm: Term => Boolean = isConst
    override val isParameter: Term => Boolean = isParam

    var definingTerm: Term = _
  }
  protected object CongrClass{
    def apply(valueId: ValueId, leader: Term, definingTerm: Term): CongrClass = {
      val congrCls = CongrClass(valueId, leader)
      congrCls.definingTerm = definingTerm
      congrCls 
    }
  }

  protected class VNTables(override val congrClasses: CongrClassesTable[Term], override val valueNumbers: ValueIds[Term])
    extends VNTablesTerms with VNTablesTrait[Term](congrClasses, valueNumbers) {
    override protected val newCongrClass: (ValueId, Term) => CongruenceClassTerms = CongrClass.apply
  }
  private object VNTables{
    def apply(): VNTables = new VNTables(CongrClassesTable[Term](), ValueIds[Term]())
  }


  protected var vnTables: VNTables = VNTables()
  

  private enum Phase:
    case initial
    case repetition

  private var phase: Phase = _


  private var currentRelationName: Name = _
  private var currentBodyIndex: Int = -1

  def printResults(): Unit = {
    if (!printVNResults) return
    println(s"Results from Relation ${oldRelation.name.name} body $currentBodyIndex")
    vnTables.printResults()
  }

  def printStatistics(input: Module, output: Module): Unit = {
    if (!printVNStatistics) return
    val stats = VNStatistics(input, output)
    stats.printStatistics()
  }

  var currentIteration: Int = 0
  def printResultsRelations(): Unit = {
    if (!printVNResults) return
    println(s"Results from VN of Relations after iteration $currentIteration")
    vnTablesRelations.printResults()
  }


  private var relations: Map[String,Relation] = _  // used to access analysis results of params of other relations


  def valueNumbering(module: ir.Module): ir.Module = visitModule(module)

  override def visitModule(module: Module): Module = {
    if (printVNResults) println(s"before VN: \n$module\n")
    relations = module.relations

    phase = Phase.initial // in initial phase congrClass is empty -> it can be assumed that all saved Vars are bound
    val tempResult = super.visitModule(module)

    printResultsRelations()

    phase = Phase.repetition // repeat with previous analysis results and rewritten bodies
    val result = repetitionPhase(tempResult)

    if (printVNResults) println(s"after VN: \n$result")

    printStatistics(module, result)
    result
  }

  @tailrec
  private def repetitionPhase(module: Module): Module = {
    oldVNTablesRelations = vnTablesRelations
    vnTablesRelations = new VNTablesRelations(CongrClassesTable[Relation](), ValueIds[Relation]())
    currentIteration += 1
    val result = super.visitModule(module)
    printResultsRelations()
    val typechecker = new IRTypechecker{}
    typechecker.checkProgram(Seq(result))
    if (result != module && useFixPointIteration){
      return repetitionPhase(result)
    }
    else {
      return result
    }
  }

  protected def updateCongrClassIfNecessary(vn: ValueId, t: Term, updateDefTermIfNecessary: Boolean = false): Unit = {
    validBody &= vnTables.updateCongrClassIfNecessary(vn, t, updateDefTermIfNecessary)
  }

  private def updateValueNumbersAndCongrClassesTerms(fromId: ValueId, toId: ValueId): Unit = {
    validBody &= vnTables.updateValueNumbersAndCongrClasses(fromId, toId)
  }

  private def updateValueNumbersAndCongrClassesTerms(t: Term, toId: ValueId): Unit = {
    validBody &= vnTables.updateValueNumbersAndCongrClasses(t, toId)
  }

  protected def getIdOf(t: Term): ValueId = vnTables.getIdOf(t)

  protected def normalize(term: Term): Term = term

  protected def isConst(term: Term): Boolean = term match {
    case Cast(t, ty) => isConst(t)
    case _ => false
  }


  private var currentRelationParams: Seq[Name] = Seq()

  protected def isParam(t: Term): Boolean = t match {
    case vari@Var(_) => currentRelationParams.contains(vari.name)
    case _ => false
  }



  private def joinParams(relation: Relation): Unit = {
    val leaders = relation.params.flatMap { param =>
      val leaders_param = relation.bodies.map { body =>
        val bodyVNTables = body.getAnalysisResult(BodyVNKey).get.vnTables
        val vn = bodyVNTables.getIdOf(Var(param.name))
        bodyVNTables.getCongrClassOf(vn).leader
      }
      leaders_param match {
        case t::tail if (leaders_param.forall(_ == t) && isConst(t)) => Seq(param.name -> leaders_param.head)
        case _ => Seq()
      }
    }
    relation.storeAnalysisResult(ParamVNResults(getResultsFromRelation(relation) ++ leaders))
  }


  private def getResultsFromRelation(relation: Relation): Map[ParamName, Term] = {
    relation.getAnalysisResult(ParamVNKey).getOrElse(ParamVNResults(Map())).paramLeaders
  }
  

  private def saveResultsInRelation(newRelation: Relation): Unit = {
    newRelation.storeAnalysisResult(ParamVNResults(getResultsFromRelation(oldRelation)))
    relations = relations + (oldRelation.name.name -> newRelation)
    joinParams(newRelation)
  }

  override def visitRelation(relation: Relation): Seq[Relation] = {
    oldRelation = relation
    currentRelationParams = relation.params.map(_.name)
    currentBodyIndex = -1

    val newRelation = super.visitRelation(relation).head
    saveResultsInRelation(newRelation)
    VNs_Bodies = ValueIds[Body]()

    valueNumberRelations(newRelation)
  }


  protected var validBody: Boolean = _


  private def setTables(body: Body): Unit = phase match {
    case Phase.initial =>
      // reset congrClasses (otherwise not known when variables are unbound)
      vnTables = VNTables()
      VNs_Atoms = ValueIds[Atom]()
    case Phase.repetition =>
      val bodyVNTables = body.getAnalysisResult(BodyVNKey).get.vnTables
      vnTables = bodyVNTables.asInstanceOf[VNTables]
      VNs_Atoms = ValueIds[Atom]() // no need to propagate old analysis results -> remove duplicates again
  }


  override def visitBody(body: Body): Seq[Body] = {
    currentBodyIndex += 1
    setTables(body)
    validBody = true // body is invalid if for example found to contain Eq(lhs,rhs) with lhs and rhs constant and lhs != rhs

    val newBody = super.visitBody(body).head

    if (!validBody) {
      return Seq()
    }

    printResults()

    // remember analysis results in body
    newBody.storeAnalysisResult(BodyVNResults(vnTables))

    return valueNumberBodies(newBody)
  }


  override def visitAtom(atom: Atom): Seq[Atom] = {
    val newAtom = atom match {
      case Eq(vari@Var(_), e, false) if vari.mode.isBinding =>
        // in case a redundant binding is found it will be removed unless it belongs to a parameter
        valueNumberVar(vari, e, dontRemove = isParam(vari))
      case Eq(e, vari@Var(_), false) if vari.mode.isBinding =>
        valueNumberVar(vari, e, dontRemove = isParam(vari))
      case Eq(vari@Var(_), e, false) =>
        // not removed (unless trivial) since non-binding Eq is comparison that might reduce number of solutions; but remember equality
        valueNumberVar(vari, e, dontRemove = true)
      case Eq(e, vari@Var(RefByName(Name(_))), false) =>
        valueNumberVar(vari, e, dontRemove = true)

      case call@Call(_, _, false) =>
        val Call(ref, args, b) = treatBindingsInCall(call)
        val newRef = visitRef(ref)
        Seq(Call(newRef, args, b))

      case Call(ref, args, b) =>
        val relation = relations(ref.name.name)
        val newRef = visitRef(ref)
        Seq(Call(newRef, args, b))

      case call@ExtensionalCall(_, _, false) => Seq(treatBindingsInExtensionalCall(call))

      case _ => super.visitAtom(atom)
    }
    valueNumberAtoms(newAtom)
  }


  /** replaces term with Var if possible */
  override def visitTerm(term: Term): Seq[Term] = {
    if (isConst(term)) return Seq(term) // don't replace constant terms and no need to normalize them
    if (vnTables.isCongrClassContained(getIdOf(term)) && isAllowedToReplace(term)) {
      return Seq(vnTables.getReplacement(term))
    }

    val newTerm = super.visitTerm(term).head
    newTerm.typ = term.typ

    val newTermId: ValueId = getIdOf(newTerm)
    val termId: ValueId = getIdOf(term)

    if (newTermId != termId){
      // ids not equal but terms are equal because newTerm was obtained by rewriting term -> should have same id
      updateValueNumbersAndCongrClassesTerms(termId, newTermId)
    }

    val normalizedTerm = normalize(newTerm)
    if (!vnTables.isValNumContained(normalizedTerm)){ // normalizedTerm not seen before
      vnTables.updateValNum(normalizedTerm, newTermId)
      if (vnTables.isCongrClassContained(newTermId)) {
        updateCongrClassIfNecessary(newTermId, normalizedTerm)
      }
    }
    else {
      // normalized term already has an id -> update term and newTerm to that id
      val normId = getIdOf(normalizedTerm)
      if (vnTables.isCongrClassContained(newTermId)) {
        updateCongrClassIfNecessary(newTermId, normalizedTerm)
      }
      if (newTermId != normId) {
        updateValueNumbersAndCongrClassesTerms(newTermId, normId)
      }
      if (vnTables.isCongrClassContained(normId) && isAllowedToReplace(newTerm)) {
          return Seq(vnTables.getReplacement(normalizedTerm))
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
    if (isConst(vnTables.getReplacement(newTerm)) && isConst(vnTables.getReplacement(newVari)) &&
      vnTables.getReplacement(newTerm) != vnTables.getReplacement(newVari)) {
      validBody = false
      return Seq(Eq(newVari,newTerm))
    }

    val termId: ValueId = getIdOf(newTerm)
    updateValueNumbersAndCongrClassesTerms(newVari, termId)

    if (vnTables.isCongrClassContained(termId)) {
      // remove "Assignment" or replace term
      if (dontRemove || phase == Phase.repetition) { // since only in 1st pass known that variable already computed/bound
        generateEqIfNecessary(newVari, newTerm)
      }
      else {
        Seq()
      }
    }

    else {
      // if term is a constant then use it as leader of its congruence class
      if (isConst(newTerm)) {
        vnTables.addCongrClass(CongrClass(termId, newTerm, newTerm))
        if (!dontRemove) return Seq() // remove binding of constant -> usages of var are replaced with constant
      }
      else {
        vnTables.addCongrClass(CongrClass(termId, newVari, newTerm))
        updateCongrClassIfNecessary(termId, newTerm, updateDefTermIfNecessary = true)
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


  private def treatBindingsInCall(call: Call): Call = {
    val Call(ref, args, neg) = call
    val relation = relations(ref.name)
    val newArgs: Seq[Arg] = args.zipWithIndex.map {
      case (TermArg(arg), idx) =>
        val paramName = relation.params(idx).name
        val paramLeaders = getResultsFromRelation(relation)
        val newArg = if (paramLeaders.contains(paramName)) {
          treatBinding(arg, paramLeaders(paramName))
        } else {
          conservativeBinding(arg)
        }
        TermArg(newArg)
      case (arg,_) => visitArg(arg).head
    }
    return Call(ref, newArgs, neg)
  }


  protected def treatBindingsWithIndex(args: Seq[Arg], terms: Seq[Term]): Seq[Arg] = args.zipWithIndex.map {
    case (TermArg(arg), idx) =>
      val newArg = treatBinding(arg, terms(idx))
      TermArg(newArg)
    case (arg, _) => visitArg(arg).head
  }


  /** treats equality of a term passed as am argument (to for example a Call) and another term 
   *
   * Makes sure given argument term and all terms with its value number get same value number as the other given term.
   * Also, makes sure corresponding congruence class is updated if necessary.
   * Returns replacement for given argument term.
   *
   * @param arg [[Term]] to give value number
   * @param term [[Term]] that is equal to [[arg]]
   * @return [[Term]] with which [[arg]] is replaced
   */
  protected def treatBinding(arg: Term, term: Term): Term = {
    val newArg = if (isParam(arg)) arg else visitTerm(arg).head
    val vn = getIdOf(term)
    if (vnTables.isCongrClassContained(vn)) {
      updateCongrClassIfNecessary(vn, newArg)
    }
    else {
      vnTables.addCongrClass(CongrClass(vn, newArg, term))
      updateCongrClassIfNecessary(vn, term)
    }
    updateValueNumbersAndCongrClassesTerms(newArg, vn)
    if (!isParam(newArg) && isAllowedToReplace(newArg)) return vnTables.getReplacement(newArg)
    else return newArg
  }



  private def treatBindingsInExtensionalCall(call: ExtensionalCall): ExtensionalCall = {
    val ExtensionalCall(ref, args, neg) = call
    val newArgs: Seq[Arg] = args.flatMap(visitArg)
    ExtensionalCall(ref, newArgs, neg)
  }


  override def visitArg(arg: Arg): Seq[Arg] = arg match {
    case TermArg(vari@Var(_)) if vari.mode.isBinding =>   // add binding vars to maps
      Seq(TermArg(conservativeBinding(vari)))
    case _ => super.visitArg(arg)
  }

  protected def conservativeBinding(term: Term): Term = { // conservative assumption that not equal to any known terms
    val newTerm = if (isParam(term)) term else visitTerm(term).head
    val id = getIdOf(newTerm)
    // in the 1st pass: binding var becomes leader of its new congr class;
    // in 2nd pass: vari was replaced with leader -> newTerm that was leader becomes new leader
    vnTables.addCongrClass(CongrClass(id, newTerm, newTerm))
    newTerm
  }



  // +++ VN of Atoms +++

  private var VNs_Atoms = ValueIds[Atom]()

  
  protected def normalizeAtom(atom: Atom): Seq[Atom] = atom match {
    case Eq(lhs, rhs, false) if lhs == rhs => Seq()
    case Eq(lhs, rhs, true) if isConst(lhs) && isConst(rhs) && lhs != rhs => Seq()
    case Eq(Var(lhs), Var(rhs), true) if lhs == rhs => validBody = false; Seq(atom)
    case Eq(lhs, rhs@Var(_), false) if rhs.mode.isBinding => Seq(Eq(rhs, lhs, false))
    case Eq(lhs, rhs, bool) if getIdOf(lhs) > getIdOf(rhs) && !lhs.mode.isBinding  => Seq(Eq(rhs, lhs, bool))
    case _ => Seq(atom)
  }


  private def valueNumberAtoms(atomSeq: Seq[Atom]): Seq[Atom] = {
    if (atomSeq.isEmpty) return atomSeq
    val atom = normalizeAtom(atomSeq.head) match {
      case h :: _ => h
      case _ => return Seq()
    }

    if (VNs_Atoms.contains(atom)){
      return Seq()
    }
    else {
      val vn = VNs_Atoms.getIdOf(atom)
      return Seq(atom)
    }
  }


  // +++ VN of Bodies +++

  private var VNs_Bodies = ValueIds[Body]()

  protected def normalizeBody(body: Body): Seq[Body] = Seq(body) // TODO

  private def valueNumberBodies(bodyInput: Body): Seq[Body] = {
    val body = normalizeBody(bodyInput) match {
      case h :: _ => h
      case _ => return Seq()
    }

    if (VNs_Bodies.contains(body)) {
      return Seq()
    }
    else {
      val vn = VNs_Bodies.getIdOf(body)
      return Seq(body)
    }
  }



  // +++ VN of Relations +++

  private var vnTablesRelations = new VNTablesRelations(CongrClassesTable[Relation](), ValueIds[Relation]())

  private var oldVNTablesRelations: VNTablesRelations = vnTablesRelations // saved for replacement in repetition phase

  private var oldRelation: Relation = _

  protected def normalizeRelation(relation: Relation): Seq[Relation] = Seq(relation) // TODO

  private def valueNumberRelations(relationInput: Relation): Seq[Relation] = {
    val relation = normalizeRelation(relationInput) match {
      case h :: _ => h
      case _ => return Seq()
    }

    // make sure that rewritten relation and old relation are equal (i.e. get same value number)
    if (vnTablesRelations.isValNumContained(oldRelation)) {
      val oldVN = vnTablesRelations.getIdOf(oldRelation)
      vnTablesRelations.updateValueNumbersAndCongrClasses(relation,oldVN)
    }

    val vn: ValueId = vnTablesRelations.getIdOf(relation)

    if (vnTablesRelations.isCongrClassContained(vn)) {
      return Seq()
    }
    else {
      vnTablesRelations.addCongrClass(vn, relation)
      return Seq(relation)
    }
  }


  override def visitRef[Target](ref: Ref[Target]): Ref[Target] = ref.target match {
    case Some(rel : Relation) if relations.contains(ref.name.name) =>
      val relation = relations(ref.name.name)
      // in repetition phase it might happen that relation not in congrClass anymore -> used tables of previous iteration
      val newName = oldVNTablesRelations.getReplacement(relation).name
      val newRef = RefByName[Relation](newName)
      newRef.target = Some(rel)
      newRef.asInstanceOf[Ref[Target]]
    case _ => ref
  }


}
