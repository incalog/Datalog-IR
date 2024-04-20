package inca.ir.valueNumbering

import inca.ir
import inca.ir.*

import scala.collection.mutable
import inca.ir.visitors.IRVisitor


/** wraps parameters for value numbering */
case class ConfigVN(normalize: Boolean = false,
                    occurrencesBeforeRemoved: Int = 0,
                   )

/** for value numbering constructs from BaseIR */
trait BaseValueNumbering(config: ConfigVN = ConfigVN()) extends IRVisitor {

  case class CongruenceClass(valueId: ValueId, var leader: Term, definingTerm: Term) {
    override def toString: String =
      s"Congruence Class: Id = $valueId, leader = $leader, definingTerm = $definingTerm"

    def changeLeaderIfNecessary(t: Term): Unit = { // also prevents type errors since in second pass otherwise might propagate unbound Vars
      if (isConst(t)) leader = t
      if (t.vars.isEmpty && !isConst(leader)) leader = t
    }
  }

  private val congrClasses: mutable.Map[ValueId, CongruenceClass] = mutable.Map()
  private val valueNumbers: ValueIds[Term] = new ValueIds() // Map[Term, ValueId]

  private def getCongrClassOf(t: Term): CongruenceClass = congrClasses(valueNumbers(t))
//  def getReplacementTerm(t: Term): Term = getCongrClassOf(t).leader
  protected def getReplacementTerm(t: Term): Term = {
    val id = valueNumbers(t)
    val leader = congrClasses(id).leader
    if (t.vars.isEmpty && leader.vars.nonEmpty){ // for edge case in 2nd phase in which const was replaced with (an unbound) var TODO other fix? (merging congrClasses would fix...)
      return t
    }
    else{
      leader.typ = t.typ // otherwise always used the type that leader had when saving it
      return leader
    }
  }

  private enum Phase:
    case initial
    case repetition
  private var phase: Phase = _

//  var count: Map[valueId, Int] = Map()   // remembers how often term with id has occurred


  def valueNumbering(module: ir.Module): ir.Module = {
    visitModule(module)
  }

  protected def getIdOf(t: Term): ValueId = valueNumbers.getIdOf(t)

  protected def normalize(term: Term): Term = term

  protected def isConst(term: Term): Boolean = false


  def printResults(): Unit = {
    println(s"Results from Relation $currentRelationName body $currentBodyIndex")
    println(s"CongruenceClasses:")
    println("\t" + congrClasses.mkString("\n\t") + "\n")
    valueNumbers.printResults()
  }


  private def newVar(name: Name, ty: Option[TermType] = None): Var = { // currently not used
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
    println(s"$currentRelationName: body $currentBodyIndex after first iteration\n{" + newBody + "\t}\n")
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
      treatBindingInEq(vari, e, dontRemove = isParam(vari), vari.typ) // in case a redundant binding is found it will be removed unless it belongs to parameter
    case Eq(e, vari@Var(_), false) if vari.mode.isBinding =>
      treatBindingInEq(vari, e, dontRemove = isParam(vari), vari.typ)
    case Eq(vari@Var(_), e, false) =>
      // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality
      treatComparisonEq(vari, e, vari.typ)
    case Eq(e, vari@Var(RefByName(Name(_))), false) =>
      treatComparisonEq(vari, e, vari.typ)

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
//    if isParam(term) then return Seq(term) // dont replace params -> this reduces number of replacements too much
//    if congrClasses.contains(valueNumbers(term)) then return Seq( getReplacementTerm(term) )

    val newTerm = super.visitTerm(term).head
    if term.typ.nonEmpty then newTerm.typed(term.typ.get)

    val termId: ValueId = valueNumbers(newTerm)
    if (termId != valueNumbers(term)){
      // ids not equal but terms are equal because newTerm was obtained by rewriting term
      // -> should have same id TODO merge congrClasses so that other terms with same ids also get same other id (?)
      valueNumbers.update(term,termId) // updating leader shouldnt be necessary since newTerm already processed and should have higher priority to be leader
    }

    // prevent terms that contain Vars with unknown value from being replaced (while not preventing Vars from being replaced)
    if (congrClasses.contains(termId) && (newTerm.vars.isEmpty || newTerm.isInstanceOf[Var])){
        return Seq(getReplacementTerm(newTerm))
    }
    else {
      val normalizedTerm = normalize(newTerm)
      if (!valueNumbers.contains(normalizedTerm)){ // normalizedTerm not seen before
        valueNumbers.update(normalizedTerm, termId)
        if (congrClasses.contains(termId)) congrClasses(termId).changeLeaderIfNecessary(normalizedTerm)
      }
      else {
        // normalized term already has an id -> update term and newTerm to that id TODO merge all with termID (?) (alternatively all with normId to termId)
        val normId = valueNumbers(normalizedTerm) // TODO normID might be termID
        valueNumbers.update(term, normId)
        valueNumbers.update(newTerm, normId)

        // if normalized term already has an id, check whether the corresponding leader can be replaced
        if (congrClasses.contains(normId)) {
          congrClasses(normId).changeLeaderIfNecessary(term)
          congrClasses(normId).changeLeaderIfNecessary(newTerm)
          if (newTerm.vars.isEmpty || newTerm.isInstanceOf[Var]) {
            return Seq(getReplacementTerm(normalizedTerm))
          }
        }

      }
      return Seq(normalizedTerm)
    }
  }


  private def treatBindingInEq(vari: Var, e: Term, dontRemove: Boolean = false, typ: Option[TermType]): Seq[Atom] = {
   valueNumberVar(vari,e,dontRemove,typ)
  }

  private def treatComparisonEq(vari: Var, t: Term, typ: Option[TermType]): Seq[Atom] = {
//    val visitedVari = visitTerm(vari).head
//    val newEqSeq = visitedVari match // vari might need to be replaced when Eq is a comparision
//      case newVari@Var(_) => valueNumberVar(newVari, t, dontRemove = true, typ) // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality
//      case other => visitTerm(t).head match {
//        case vari2@Var(_) => treatComparisonEq(vari2,other,typ) // t might be a Var if first case was taken in visitAtom
//        case other2 => Seq(Eq(other,other2)) //valueNumberVar(vari, t, dontRemove = true, typ)
//      }
    val res1 = valueNumberVar(vari, t, dontRemove = true, typ)
    val res2 = t match { // t might be a Var if first case was taken in visitAtom
      case variRhs: Var => valueNumberVar(variRhs, vari, dontRemove = true, typ)
      case _ => Seq()
    }
    res1


    //    return newEqSeq
  }


  private def valueNumberVar(vari: Var, t: Term, dontRemove: Boolean = false, typ: Option[TermType]): Seq[Eq] = {
    // for case: in the 2nd pass might be replaced with vari and if vari is a param then new ill-typed Eq will not be removed
    val tempTerm = visitTerm(t).head
    val newTerm = if (tempTerm == vari && t != vari) || isParam(t) then t else tempTerm
    val newVari = if !(isParam(vari)) then visitTerm(vari).head else vari

    val dontRemove2 = dontRemove || phase == Phase.repetition

    val termId: ValueId = getIdOf(newTerm)
    if (congrClasses.contains(termId)) {
      valueNumbers.update(newVari,termId)
      // TODO update leader ? -> shouldnt be necessary since term already processed and Var shouldnt have higher priority to be leader
      //count = count.updated(termId, count (termId) + 1)

      // remove "Assignment" or replace term
      if (dontRemove2) { // used other dontRemove here since only in 1st pass known that already computed ( & irrelevant if var contained) TODO ?
        Seq( Eq(newVari, newTerm) )
      }
      else {
        Seq()
      }
    }
    else if (congrClasses.contains(valueNumbers(newVari))){
      valueNumbers.update(newTerm, valueNumbers(newVari))
      // update congrClass if newTerm is a const
      getCongrClassOf(newVari).changeLeaderIfNecessary(newTerm)
      Seq(Eq(newVari,newTerm))
    }
    else {
      valueNumbers.update(newVari, termId)
      //count += (termId,1)

      // if term is a constant then use it as leader of its congruence class
      // if no Vars are left in term then also use it -> leads to removal of more redundant atoms of the form T == T
      // TODO these atoms could also be removed statically by other means
      if (isConst(newTerm) || newTerm.vars.isEmpty) {
        congrClasses.update(termId, CongruenceClass(termId, newTerm, newTerm))
        if !dontRemove then return Seq() // remove binding of constant -> usages of var are replaced with constant
      }
      else {
        congrClasses.update(termId, CongruenceClass(termId, newVari, newTerm))
        congrClasses(termId).changeLeaderIfNecessary(newTerm)
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
