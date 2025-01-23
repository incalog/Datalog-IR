package inca.ir.valueNumbering.BaseVN

import inca.ir
import inca.ir.*
import inca.ir.typing.IRTypechecker
import inca.ir.valueNumbering.VNTables.*
import inca.ir.valueNumbering.{BodyVNKey, BodyVNResults, ParamName, ParamVNKey, ParamVNResults, VNStatistics}
import inca.ir.visitors.IRVisitor



/** for value numbering constructs from BaseIR */
trait BaseValueNumberingTerms extends IRVisitor {
  // config
  def normalizeDoubles: Boolean = false
  def useDefiningTerm: Boolean = false
  def useFixPointIteration: Boolean = true
  def printVNResults: Boolean = true
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


  private[BaseVN] enum Phase:
    case initial // in initial phase congrClass is empty -> it can be assumed that all saved Vars are bound
    case repetition // repeat with previous analysis results and rewritten bodies

  private[BaseVN] var phase: Phase = Phase.initial

  protected var oldRelation: Relation = _
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


  private[BaseVN] var relations: Map[String,Relation] = _  // used to access analysis results of params of other relations

  

  override def visitModule(module: Module): Module = {
      relations = module.relations

      val tempResult = super.visitModule(module)
      phase = Phase.repetition
      val result = repetitionPhase(tempResult)
    
      result
  }


  private[BaseVN] def repetitionPhase(module: Module): Module = {
    currentIteration += 1
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

  
  protected def normalize(term: Term): Term = term match {
    case Cast(t, ty) if t.typ.get.ty == ty => t
    case _ => term
  }

  protected def isConst(term: Term): Boolean = false


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
    Seq(newRelation)
  }


  protected var validBody: Boolean = _


  private[BaseVN] def setTables(body: Body): Unit = phase match {
    case Phase.initial =>
      // reset congrClasses (otherwise not known when variables are unbound)
      vnTables = VNTables()
    case Phase.repetition =>
      val bodyVNTables = body.getAnalysisResult(BodyVNKey).get.vnTables
      vnTables = bodyVNTables.asInstanceOf[VNTables]
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

    return Seq(newBody)
  }


  override def visitAtom(atom: Atom): Seq[Atom] = atom match {
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

    case call@ExtensionalCall(_, _, false) => Seq(treatBindingsInExtensionalCall(call))

    case _ => super.visitAtom(atom)
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
  protected def isAllowedToReplace(term: Term): Boolean = (term.vars.isEmpty || term.isInstanceOf[Var])
    // Cast might contain a constant and thus be needed to constraint variables
    // (if the constants type and the type of the cast does not match it is not a constant itself)
    && !term.isInstanceOf[Cast]


  private def valueNumberVar(vari: Var, t: Term, dontRemove: Boolean = false): Seq[Eq] = {
    val newTerm = if (isParam(t)) t else visitTerm(t).head
    val newVari = if (isParam(vari)) vari else visitTerm(vari).head

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


  /** treats equality of a term passed as an argument (to for example a Call) and another term
   *
   * Makes sure given argument term and all terms with its value number get the same value number as the other given term.
   * Also, makes sure that the corresponding congruence class is updated if necessary.
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
    case TermArg(vari@Var(_)) if isParam(vari) => Seq(arg)
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


}
