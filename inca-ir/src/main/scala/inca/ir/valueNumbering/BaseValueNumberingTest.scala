package inca.ir.valueNumbering

import inca.ir
import inca.ir.*
import inca.util.Gensym

import scala.collection.mutable
import inca.ir.visitors.IRVisitor


/** wraps parameters for value numbering */
case class ConfigVN(normalize: Boolean = false,
                    occurrencesBeforeRemoved: Int = 0,
                   )

/** for value numbering constructs from BaseIR */
trait BaseValueNumbering(config: ConfigVN = ConfigVN()) extends IRVisitor {

  case class CongruenceClass(valueId: ValueId, var leader: Term, definingTerm: Term, var contents: Seq[Term]) { // contents just saved for debugging
    override def toString: String =
      s"Congruence Class: Id = $valueId, leader = $leader, definingTerm = $definingTerm, contents = $contents"

    def add(t: Term): Unit = {
      if (isConst(t)) leader = t
      contents = contents.appended(t)
    }

  }

  private val congrClasses: mutable.Map[ValueId, CongruenceClass] = mutable.Map()
  private val valueNumbers: ValueIds[Term] = new ValueIds() // Map[Term, ValueId]

  def getCongrClassOf(t: Term): CongruenceClass = congrClasses(valueNumbers(t))
  def getReplacementTerm(t: Term): Term = getCongrClassOf(t).leader

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

  // for printing
  private var currentRelationName: Name = _
  private var currentBodyIndex: Int = -1


  private var relationParams: Seq[Name] = Seq()
  private def isParam(vari: Var): Boolean = relationParams.contains(vari.name)


  override def visitRelation(relation: Relation): Seq[Relation] = {
    currentRelationName = relation.name
    relationParams = relation.params.map(_.name)
    currentBodyIndex = -1
    super.visitRelation(relation)
  }


  override def visitBody(body: Body): Seq[Body] = {
    currentBodyIndex += 1
    phase = Phase.initial
    val newBody = super.visitBody(body).head
//    println(s"$currentRelationName: body $currentBodyIndex after first iteration\n{" + newBody + "\t}")
    phase = Phase.repetition
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
      // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality and remove if duplicate of other Eq
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
//    if congrClasses.contains(valueNumbers(term)) then return Seq( getReplacementTerm(term) )

    val newTerm = super.visitTerm(term).head
    if term.typ.nonEmpty then newTerm.typed(term.typ.get)

    val termId: ValueId = valueNumbers(newTerm)
    if (termId != valueNumbers(term)){
      // ids not equal but terms are equal because newTerm was obtained by rewriting term
      // -> should have same id
      valueNumbers.update(term,termId)
    }

    if (congrClasses.contains(termId)) {
      if (newTerm.vars.isEmpty || newTerm.isInstanceOf[Var]){
        return Seq(congrClasses(termId).leader)
      }
      else {
        return Seq(newTerm)
      }
    }
    else {
      val normalizedTerm = normalize(newTerm)
      valueNumbers.update(normalizedTerm,termId)
      // add to congrClass (not needed for this to work but maybe change leader ???)
//        val normalTermId = valueNumbers(normalizedTerm)
//        congrClasses.update(normalTermId, {
//          congrClasses(normalTermId).contents = congrClasses(normalTermId).contents.appended(normalizedTerm); congrClasses(normalTermId)
//        })
      return Seq(normalizedTerm)
    }
  }


  private def treatBindingInEq(vari: Var, e: Term, dontRemove: Boolean = false, typ: Option[TermType]): Seq[Atom] = {
   valueNumberVar(vari,e,dontRemove,typ)
  }

  private def treatComparisonEq(vari: Var, t: Term, typ: Option[TermType]): Seq[Atom] = {
    val visitedVari = visitTerm(vari).head
    val newEqSeq = visitedVari match // vari might need to be replaced when Eq is a comparision
      case newVari@Var(_) => valueNumberVar(newVari, t, dontRemove = true, typ) // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality
      case other => visitTerm(t).head match {
        case vari2@Var(_) => treatComparisonEq(vari2,other,typ) // t might be a Var if first case was taken in visitAtom
        case other2 => Seq(Eq(other,other2)) //valueNumberVar(vari, t, dontRemove = true, typ)
      } // happens when propagating constants

    return newEqSeq
  }


  private def valueNumberVar(vari: Var, t: Term, dontRemove: Boolean = false, typ: Option[TermType]): Seq[Eq] = {
    // in the second pass might be replaced with vari and if vari is a param then new ill-typed Eq will not be removed
    val newTerm = if (visitTerm(t).head == vari && t != vari) then t else visitTerm(t).head
    val newVari = if !isParam(vari) then visitTerm(vari).head else vari

    val dontRemove2 = dontRemove || (newTerm.vars.nonEmpty && !newTerm.isInstanceOf[Var]) || phase == Phase.repetition // TODO for some this fixes error but for others it introduces one -> find out why

    val termId: ValueId = getIdOf(newTerm)
    if (congrClasses.contains(termId)) {
      valueNumbers.update(newVari,termId)
      // update CongruenceClass.contents not necessary (see comment on class CongruenceClass) TODO update leader ?
      congrClasses(termId).contents = congrClasses(termId).contents.appended(vari)
      //count = count.updated(termId, count (termId) + 1)

      // remove "Assignment" or replace term
      if dontRemove2 then {
        // newTerm instead of Var(Name(v)) so that what is replaced only decided in visitTerm
        Seq( Eq(newVari, newTerm) )
      }
      else {
        Seq()
      }
    }
    else if (congrClasses.contains(valueNumbers(newVari))){
      valueNumbers.update(newTerm, valueNumbers(newVari))
      // TODO update congrClass if newTerm is a const ?
      getCongrClassOf(newVari).add(newTerm)
      Seq(Eq(newVari,newTerm))
    }
    else {
      valueNumbers.update(newVari, termId)
      //count += (termId,1)

      if (isConst(newTerm)) { // if term is a constant then use it as leader of its congruence class
        congrClasses.update(termId, CongruenceClass(termId, newTerm, newTerm, Seq(newVari, newTerm))) // TODO include old term t ?
        if !dontRemove2 then return Seq() // remove binding of constant -> usages of var are replaced with constant
      }
      else {
        congrClasses.update(termId, CongruenceClass(termId, newVari, newTerm, Seq(newVari, newTerm)))
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
          congrClasses.update(id, CongruenceClass(id, vari, vari, Seq(vari)))
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
