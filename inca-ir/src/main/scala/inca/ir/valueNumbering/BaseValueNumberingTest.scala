package inca.ir.valueNumbering

import inca.ir
import inca.ir.*
import inca.util.Gensym

import scala.collection.mutable
import inca.ir.visitors.IRVisitor


/** wraps parameters for value numbering */
case class ConfigVN(normalize: Boolean = false,
                    propagateConstants: Boolean = true,
//                    occurrencesBeforeRemoved: Int = 0,
                   )

/** for value numbering constructs from BaseIR */
trait BaseValueNumbering(config: ConfigVN = ConfigVN()) extends IRVisitor {

  // TODO currently equal terms do not all get the same id e.g. a-a -> 3 but 0 -> 4 ?
  private val congrClasses: mutable.Map[ValueId, CongruenceClass] = mutable.Map()
  private val valueNumbers: ValueIds[Term] = new ValueIds() // Map[Term, ValueId]

  def getCongrClassOf(t: Term): CongruenceClass = congrClasses(valueNumbers(t))
  def getReplacementTerm(t: Term): Term = getCongrClassOf(t).leader

//  var count: Map[valueId, Int] = Map()   // remembers how often term with id has occurred

//  var gensym: Gensym = Gensym()

  // global Maps for terms
    val congrClassesGlobal: mutable.Map[ValueId,CongruenceClass] = mutable.Map()


  def valueNumbering(module: ir.Module): ir.Module = {
    val newModule = visitModule(module)
    valueNumbers.printResults()
    println("\t" + congrClasses.mkString("\n\t") + "\n")
    newModule
  }

  override def visitModule(module: Module): Module = {
    super.visitModule(module)
  }


  protected def getIdOf(t: Term): ValueId = valueNumbers.getIdOf(t)

  // is used for Var with name bindingArg and NOT for the atom
  protected def getIdOf(atom: Atom, bindigArg: Var): ValueId = valueNumbers.getIdOf(atom,bindigArg)





  private var currentRelationName: Name = _

  private var relationParams: Seq[Name] = Seq()
  private def isParam(vari: Var): Boolean = relationParams.contains(vari.name)

  private var currentBodyIndex: Int = -1

  override def visitRelation(relation: Relation): Seq[Relation] = {
    currentRelationName = relation.name
    relationParams = relation.params.map(_.name)

    currentBodyIndex = -1
    super.visitRelation(relation)
  }



  private var currentBodyVars: Seq[Var] = Seq()
//  private def isUsedInBody(t: Term): Boolean = currentBodyVars.map(_.name.name).contains(t) // TODO like this or use gensym?


  override def visitBody(body: Body): Seq[Body] = {
    congrClasses.foreach((k,congrClass) => congrClassesGlobal.update(k,congrClass)) // TODO merge congrClass

    currentBodyIndex += 1
    currentBodyVars = body.vars
    val newBodySeq2 = super.visitBody(body)
//    val newBodySeq2 = super.visitBody(body)

    // reset congrClasses (otherwise not known when variables are unbound)
    println(s"CongruenceClasses from Relation $currentRelationName body $currentBodyIndex")
    println("\t" + congrClasses.mkString("\n\t") + "\n")
    congrClasses.clear()
    newBodySeq2
  }


  private def newVar(name: Name, ty: Option[TermType] = None): Var = {
    val v = Var(RefByName(name))
    v.typ = ty
    v
  }




  protected def normalize(term: Term): Term = term

  /** replaces term with Var if possible */
  override def visitTerm(term: Term): Seq[Term] = {
    if isConst(term) then return Seq(term) // dont replace constant terms and no need to normalize them
    if congrClasses.contains(valueNumbers(term)) then return Seq( getReplacementTerm(term) )

    val newTerm = super.visitTerm(term).head
    if term.typ.nonEmpty then newTerm.typed(term.typ.get)
    val termId: ValueId = valueNumbers(newTerm)
    if (termId != valueNumbers(term)){
      // ids not equal but terms are equal because newTerm was obtained by rewriting term
      // -> should have same id
      valueNumbers.update(term,termId)
    }
    if (congrClasses.contains(termId)) {
      val res = Seq(congrClasses(termId).leader)
      res
    }
    else {
      val normalizedTerm = normalize(newTerm)
      valueNumbers.update(normalizedTerm,termId)
      // add to congrClass (not needed for this to work but maybe change leader ???)
//        val normalTermId = valueNumbers(normalizedTerm)
//        congrClasses.update(normalTermId, {
//          congrClasses(normalTermId).contents = congrClasses(normalTermId).contents.appended(normalizedTerm); congrClasses(normalTermId)
//        })
      Seq(normalizedTerm)
    }
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

    case call@Call(_, _, false) =>
      val superVisited = super.visitAtom(atom).head
      superVisited match {
        case call@Call(ref, args, false) =>
          val newCall = treatBindingsInCall(call, args)
          newCall
      }

    // TODO can equivalence of two vars not be concluded from calls?
    //  could be e.g. that b(x) :- x == 0. b(x) :- x == 2. so that b(a1), b(a2) not necessarily implies that a1 == a2
    //        val otherRelation = ref.target.get
    //        if (otherRelation.bodies.size <= 1){  // then args which are bound by call have a unique value
    //          args.map{arg =>
    //           ???
    //          }
    //        }

    case call@ExtensionalCall(_, _, false) =>
      super.visitAtom(atom).head match {
        case call@ExtensionalCall(ref, args, false) => treatBindingsInCall(call, args)
      }

    case _ => super.visitAtom(atom)
  }

  protected def isConst(term: Term): Boolean = false

  private def treatBindingInEq(vari: Var, e: Term, dontRemove: Boolean = false, typ: Option[TermType]): Seq[Atom] = {
   valueNumberVar(vari,e,dontRemove,typ)
  }

  private def treatComparisonEq(vari: Var, t: Term, typ: Option[TermType]): Seq[Atom] = {
    val newEqSeq = visitTerm(vari).head match // vari might need to be replaced when Eq is a comparision
      case newVari@Var(_) => valueNumberVar(newVari, t, dontRemove = true, typ) // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality
      case other => t match {
        case vari2@Var(_) => treatComparisonEq(vari2,other,typ) // t might be a Var if first case was taken in visitAtom
        case _ =>  valueNumberVar(vari, t, dontRemove = true, typ)
      } // happens when propagating constants

    if newEqSeq.isEmpty then return Seq()
    val newEq = newEqSeq.head
    val newAtomSeq = {
        Seq(newEq)
    }
    newAtomSeq

  }

  private def valueNumberVar(vari: Var, t: Term, dontRemove: Boolean = false, typ: Option[TermType]): Seq[Eq] = {
    val newTerm = visitTerm(t).head
    val varName = vari.name

    val termId: ValueId = getIdOf(newTerm)
    if (congrClasses.contains(termId)) {
      valueNumbers.update(vari,termId)
      // update CongruenceClass.contents not necessary (see comment on class CongruenceClass) TODO update leader ?
      congrClasses(termId).contents = congrClasses(termId).contents.appended(vari)
      //count = count.updated(termId, count (termId) + 1)

      // remove "Assignment" or replace term
      if dontRemove then {
        // newTerm instead of Var(Name(v)) so that what is replaced only decided in visitTerm
        Seq( Eq(vari, newTerm) )
      }
      else {
        Seq()
      }
    }
    else {
      valueNumbers.update(vari, termId)
      if (isConst(newTerm) && this.config.propagateConstants) {
        congrClasses.update(termId, CongruenceClass(termId, newTerm, newTerm, Seq(vari, newTerm))) // TODO include old term t ?
        if !dontRemove then return Seq() // remove binding of constant -> usages of var are replaced with constant
      }
      else congrClasses.update(termId,CongruenceClass(termId,vari,newTerm,Seq(vari,newTerm)))

      //count += (termId,1)

      // return with newTerm
      Seq( Eq(vari, newTerm) )
    }
  }



  private def treatBindingsInCall(call: Call | ExtensionalCall, args: Seq[Arg]): Seq[Atom] = {
    var isBinding = false
    val newArgs: Seq[Arg] = args.map {
      case t@TermArg(term) => term match {

        case vari@Var(RefByName(variName)) if vari.mode.isBinding => // add binding vars to maps
          isBinding = true
          val bindingCallId = getIdOf(call, vari)           // same calls except currently binding var should have same ValNum in different Relations
//            congrClasses.update(bindingCallId,CongruenceClass(bindingCallId,vari,vari,Seq(vari))) // TODO check whether already known (?) -> in general wrong to conclude equality (in same body)
//            valueNumbers.update(vari,bindingCallId)
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
