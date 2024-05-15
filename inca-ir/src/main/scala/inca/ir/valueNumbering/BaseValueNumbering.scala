package inca.ir.valueNumbering

import inca.ir
import inca.ir.*

import scala.collection.mutable
import inca.ir.visitors.IRVisitor


/** wraps parameters for value numbering */
case class ConfigVN(normalize: Boolean = true,
//                    occurrencesBeforeRemoved: Int = 0,
                    useDefiningTerm: Boolean = false
                   )

/** for value numbering constructs from BaseIR */
trait BaseValueNumbering(config: ConfigVN = ConfigVN()) extends IRVisitor {

  private case class CongruenceClass(valueId: ValueId, var leader: Term, var definingTerm: Term) {
    override def toString: String =
      s"Congruence Class: Id = $valueId, leader = $leader, definingTerm = $definingTerm"

    def changeLeaderIfNecessary(t: Term): Unit = { // also prevents type errors since in second pass otherwise might propagate unbound Vars
//      val oldLeader = leader
      if (isConst(t)) leader = t
      if (!isParam(leader) && !isConst(leader) && isParam(t)) leader = t
//      val newLeader = leader
//      println("valueId = " + valueId + ", oldLeader = " + oldLeader + ", newLeader = " + newLeader)
    }

    def changeDefTermIfNecessary(t: Term): Unit = {
      if (isConst(t)) definingTerm = t
      else if (definingTerm.isInstanceOf[Var] && !t.isInstanceOf[Var]) definingTerm = t // resembles case that CongruenceClass was initially created for Var bound in Call
    }

  }

  private val congrClasses: mutable.Map[ValueId, CongruenceClass] = mutable.Map()
  private val valueNumbers: ValueIds[Term] = new ValueIds() // Map[Term, ValueId]

  private def getCongrClassOf(t: Term): CongruenceClass = congrClasses(valueNumbers(t))

  protected def getReplacementTerm(t: Term): Term = {
    val leader = getCongrClassOf(t).leader
    if (phase == Phase.repetition && t.vars.isEmpty && leader.vars.nonEmpty){
      // for case in 2nd phase in which term was replaced with an unbound var TODO other fix below?
      // e.g. without this param == 1 ~> param == param
      return t
    }
    else{
      leader match {
        case vari@Var(_) => newVar(vari.name,t.typ)
        case _ => leader
      }
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
      congrClasses(toId).changeLeaderIfNecessary(term)
      congrClasses(toId).changeDefTermIfNecessary(term)
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
    }
    else if (!congrClasses.contains(fromId) && congrClasses.contains(toId)) {
      updateToCongrClass = true
    }
    valueNumbers.getAllWithId(fromId).foreach(t =>
      valueNumbers.update(t,toId)
      if (updateToCongrClass) {
        congrClasses(toId).changeLeaderIfNecessary(t)
        congrClasses(toId).changeDefTermIfNecessary(t) // necessary ?
      }
    )
    congrClasses.remove(fromId)
  }


  private enum Phase:
    case initial
    case repetition
  private var phase: Phase = _

//  var count: Map[valueId, Int] = Map()   // remembers how often term with id has occurred

  override def visitModule(module: Module): Module = {
    println(s"before VN: \n$module\n")
    val result = super.visitModule(module)
    println(s"after VN: \n$result")
    result
  }

  def valueNumbering(module: ir.Module): ir.Module = {
    visitModule(module)
  }

  protected def getIdOf(t: Term): ValueId = valueNumbers.getIdOf(t)

  protected def normalize(term: Term): Term = term

  protected def isConst(term: Term): Boolean = false


  def printResults(): Unit = {
    println(s"Results from Relation $currentRelationName body $currentBodyIndex")
    println(s"Congruence Classes Info:")
    println("\t" + congrClasses.mkString("\n\t") + "\n")
    valueNumbers.printResults()
  }


  private def newVar(name: Name, ty: Option[TermType] = None): Var = {
    val v = Var(RefByName(name))
    v.typ = ty
    v
  }

  // for printing results
  private var currentRelationName: Name = _
  private var currentBodyIndex: Int = -1


  private var relationParams: Seq[Name] = Seq()
  private def isParam(t: Term): Boolean = t match {
    case vari@Var(_) => relationParams.contains(vari.name)
    case _ => false
  }


  override def visitRelation(relation: Relation): Seq[Relation] = {
    currentRelationName = relation.name
    relationParams = relation.params.map(_.name)
    currentBodyIndex = -1
    super.visitRelation(relation)
  }


  override def visitBody(body: Body): Seq[Body] = {
    currentBodyIndex += 1

    phase = Phase.initial // in initial phase congrClass is empty -> it can be assumed that all seen Vars are bound
    val newBody = super.visitBody(body).head
//    println(s"$currentRelationName: body $currentBodyIndex after first iteration\n{" + newBody + "\t}\n")
    phase = Phase.repetition // in repetition phase previous results are used to discover more equalities -> cant be assumed that all seen Vars are bound
    val newerBodySeq = super.visitBody(newBody)

    printResults()

    // reset congrClasses (otherwise not known when variables are unbound)
    congrClasses.clear()
    valueNumbers.clear()
    newerBodySeq
  }


  override def visitAtom(atom: Atom): Seq[Atom] = atom match {
    case Eq(vari@Var(_), e, false) if vari.mode.isBinding =>
      // in case a redundant binding is found it will be removed unless it belongs to parameter
      treatBindingInEq(vari, e, dontRemove = isParam(vari))
    case Eq(e, vari@Var(_), false) if vari.mode.isBinding =>
      treatBindingInEq(vari, e, dontRemove = isParam(vari))
    case Eq(vari@Var(_), e, false) =>
      // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality
      treatComparisonEq(vari, e)
    case Eq(e, vari@Var(RefByName(Name(_))), false) =>
      treatComparisonEq(vari, e)

    case call@Call(_, _, false) =>  // TODO when can equivalence of two vars be concluded from calls?
      super.visitAtom(atom).head match {
        case call@Call(ref, args, false) => treatBindingsInCall(call, args)
      }
    case call@ExtensionalCall(_, _, false) =>
      super.visitAtom(atom).head match {
        case call@ExtensionalCall(ref, args, false) => treatBindingsInCall(call, args)
      }

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

    val normalizedTerm = normalize(newTerm)//(true)
//      normalizedMem.update(newTerm,normalizedTerm)
    if (!valueNumbers.contains(normalizedTerm)){ // normalizedTerm not seen before
      valueNumbers.update(normalizedTerm, newTermId)
      if (congrClasses.contains(newTermId)) {
        congrClasses(newTermId).changeLeaderIfNecessary(normalizedTerm)
        congrClasses(newTermId).changeDefTermIfNecessary(normalizedTerm)
      }
    }
    else {
      // normalized term already has an id -> update term and newTerm to that id
      val normId = valueNumbers(normalizedTerm)
      if (newTermId != normId) updateValueNumbersAndCongrClasses(newTermId, normId)

      if (congrClasses.contains(normId) && isAllowedToReplace(newTerm)) {
          return Seq(getReplacementTerm(normalizedTerm))
        }
    }

  // TODO tried finding indicators which term is "better" (probably should implement term.size if want to use something like that)
  //    -> sometimes leads to less equalities being found...
  //    maybe normalize once without defTerm and once with defterm and choose better one for program but save both in VN maps???
    //      return Seq(
    //        if (isConst(newTerm) || newTerm.isInstanceOf[Var] || (normalizedTerm.vars.size >= newTerm.vars.size))
    //          && !isConst(normalizedTerm)
    //          then newTerm
    //        else normalizedTerm
    //      )

//        if (!congrClasses.contains(getIdOf(normalizedTerm))) congrClasses.update(getIdOf(normalizedTerm), CongruenceClass(getIdOf(normalizedTerm),normalizedTerm,normalizedTerm))
      return Seq(normalizedTerm)
  }

//  protected val normalizedMem: mutable.Map[Term,Term] = mutable.Map()

  // prevent terms that contain Vars with unknown value from being replaced (while not preventing Vars from being replaced)
  private def isAllowedToReplace(term: Term): Boolean = term.vars.isEmpty || term.isInstanceOf[Var]


  private def treatBindingInEq(vari: Var, e: Term, dontRemove: Boolean = false): Seq[Atom] = {
   valueNumberVar(vari,e,dontRemove)
  }

  private def treatComparisonEq(vari: Var, t: Term): Seq[Atom] = {
    // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality
    valueNumberVar(vari, t, dontRemove = true)
  }


  private def valueNumberVar(vari: Var, t: Term, dontRemove: Boolean = false): Seq[Eq] = {
//    val tempTerm = visitTerm(t).head
//    val newTerm = if (tempTerm == newVari && t != newVari && newVari.mode.isBinding) || isParam(t) then t else tempTerm
    val newTerm = if isParam(t) then t else visitTerm(t).head
    val newVari = if isParam(vari) then vari else visitTerm(vari).head

    val termId: ValueId = getIdOf(newTerm)
    if (congrClasses.contains(termId)) {
      updateValueNumbersAndCongrClasses(newVari, termId)
      //count = count.updated(termId, count (termId) + 1)

      // remove "Assignment" or replace term
      if (dontRemove || phase == Phase.repetition) { // since only in 1st pass known that already computed/bound
        Seq( Eq(newVari, newTerm) )
      }
      else {
        Seq()
      }
    }

    else {
      updateValueNumbersAndCongrClasses(newVari,termId)
      //count += (termId,1)

      // if term is a constant then use it as leader of its congruence class
      // TODO atoms of the form term == term could also be removed statically
      if (isConst(newTerm)) {
        congrClasses.update(termId, CongruenceClass(termId, newTerm, newTerm /*normalizedMem.getOrElse(newTerm,newTerm)*/))
        if !dontRemove then return Seq() // remove binding of constant -> usages of var are replaced with constant
      }
      else {
        congrClasses.update(termId, CongruenceClass(termId, newVari, newTerm /*normalizedMem.getOrElse(newTerm,newTerm)*/))
        congrClasses(termId).changeLeaderIfNecessary(newTerm)
        congrClasses(termId).changeDefTermIfNecessary(newTerm)
      }

      // return with newTerm
      Seq( Eq(newVari, newTerm) )
    }
  }


  private def treatBindingsInCall(call: Call | ExtensionalCall, args: Seq[Arg]): Seq[Atom] = {
    val newArgs: Seq[Arg] = args.map {
      case t@TermArg(term) => term match {

        case vari@Var(RefByName(variName)) if vari.mode.isBinding => // add binding vars to maps
          val id = valueNumbers.getIdOf(vari)
          congrClasses.update(id, CongruenceClass(id, vari, vari))
          t
        case _ => t
      }
      case t => t
    }
    val newCall = call match{
      case Call(ref,_,neg) => Call(ref,newArgs,neg)
      case ExtensionalCall(ref,_,neg) => ExtensionalCall(ref,newArgs,neg)
    }
    Seq(newCall)
  }


}
