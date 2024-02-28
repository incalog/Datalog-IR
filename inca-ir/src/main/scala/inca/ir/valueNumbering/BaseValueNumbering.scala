package inca.ir.valueNumbering

import inca.ir
import inca.ir.*
import inca.ir.analysis.Analyzable
//import inca.ir.extension.arithmetic.*
import inca.ir.util.SourceLocation
import inca.ir.visitors.IRVisitor


/** wraps parameters for value numbering */
case class ConfigVN(simplifyArithmetic: Boolean = false,
                    propagateConstants: Boolean = false,
                    removeTrueAtoms: Boolean = false,
                    occurrencesBeforeRemoved: Int = 0)

/** for value numbering constructs from BaseIR */
//class ValueNumbering(analysis: IRAbstractInterpreter) extends IROptimizer(analysis) {
trait BaseValueNumbering(config: ConfigVN = ConfigVN()) extends IRVisitor {

  type ValNum = String
  type Hashed = Int

  // maps for terms and atoms
  var VN: Map[String, ValNum] = Map() // String is a Name TODO Map[Name, ValNum] ?
  var hashTable: Map[Hashed, ValNum] = Map()
  var const: Map[String, Term] = Map() // remembers constant term assigned to Var with name string
  var count: Map[Hashed, Int] = Map()   // remembers how often term with hash has occurred

  var valueUnknown: Set[Term] = Set() // remembers variables that where bound in calls -> if they are compared in Eq those shouldnt be removed

  // maps for atoms
  var VNAtoms: Map[String, ValNum] = Map() // String is a Name
  var hashTableAtoms: Map[Hashed, ValNum] = Map()

  // maps for bodies
  var VNBodies: Map[String, ValNum] = Map()
  var hashTableBodies: Map[Hashed, ValNum] = Map()

  // maps for relations
  var VNRelations: Map[String, ValNum] = Map()
  var hashTableRelations: Map[Hashed, ValNum] = Map()

  // global Maps for terms
  var VNGlobal: Map[String, ValNum] = Map() // String is a Name
  var hashTableGlobal: Map[Hashed, ValNum] = Map()


  def valueNumbering(module: ir.Module): ir.Module = {
    // ugly fix for changing references from removed relations  // TODO refactor / rewrite so that relations are processed in different order
    class CallsFix extends IRVisitor {
      def fixCalls(module: Module): Module = {
        super.visitModule(module)
      }
      override def visitAtom(atom: Atom): Seq[Atom] = atom match {
        case Call(ref, args, neg) => Seq(Call(VNRelations(ref.name.name), args, neg))
//        case ExtensionalCall(ref, args, neg) => Seq(ExtensionalCall(VNRelations(ref.name.name), args, neg)) // TODO can Ext relations be analyzed?
        case _ => Seq(atom) // no further descend in AST
      }
    }

    val newModule = visitModule(module)
    new CallsFix().fixCalls(newModule)

  }

  override def visitModule(module: Module): Module = {
    VNRelations = Map()
    hashTableRelations = Map()
    super.visitModule(module)
  }

  // TODO dont use scala`s hashing function
  // Type of argument -> changed from Term to Analyzable since need to Hash Atoms like Calls too
  protected def getHashCode(elem: Term): Hashed = elem match {
    case Var(RefByName(Name(name))) if VN.contains(name) && hashTable.exists(_._2 == name) => hashTable.find(_._2 == name).head._1
    case _ => elem.hashCode()
  }

  // bindingArg is passed if hash is used for Var with this name and NOT for the atom
  protected def getHashCode(atom: Atom, bindigArg: Option[String] = None): Hashed = atom match {
    case Call(ref,args,neg) => bindigArg match {
      case Some(withoutArg) =>(Seq(ref) ++ args.filter { // there should be no false positive caused by removal because no relation name used twice
          case TermArg(Var(Name(x))) => x == withoutArg
          case _ => false
        } ++ Seq(neg)).hashCode()
      case None => (Seq(ref) ++ args ++ Seq(neg)).hashCode()
    }
    case _ => atom.hashCode()
  }


  protected def getHashCode(body: Body): Hashed = body.hashCode()

  protected def getHashCode(relation: Relation): Hashed = (relation.params ++ relation.bodies).hashCode()  // -> name of relation irrelevant


  private var relationParams: Seq[Name] = Seq()
  private def isParam(x: String): Boolean = relationParams.map(_.name).contains(x)

  override def visitRelation(relation: Relation): Seq[Relation] = {
    relationParams = relation.params.map(_.name)
    VNBodies = Map()
    hashTableBodies = Map()
    valueNumberRelations(relation)
  }

  private def valueNumberRelations(relation: Relation): Seq[Relation] = {
    // removes correctly but references need to be changed -> ugly fix above
    val newRelation = super.visitRelation(relation).head
    val x = newRelation.name.name

    val relationHash: Hashed = getHashCode(newRelation)
    if (hashTableRelations.contains(relationHash)) {
      val v: ValNum = hashTableRelations(relationHash)
      VNRelations += (x, v)
      // remove redundant body
      Seq()
    }
    else {
      val v = x
      VNRelations += (x, v)
      hashTableRelations += (relationHash, v)

      Seq(newRelation)
    }
  }

  override def visitBody(body: Body): Seq[Body] = {
    VNGlobal = VNGlobal ++ VN
    hashTableGlobal = hashTableGlobal ++ hashTable

    VN = Map()
    hashTable = Map()
    const = Map()

    VNAtoms = Map()
    hashTableAtoms = Map()
    valueNumberBodies(body)
  }

  private def valueNumberBodies(body: Body): Seq[Body] = {
    val newBody = super.visitBody(body).head
    val x = newBody.toString

    val bodyHash: Hashed = getHashCode(newBody)
    if (hashTableBodies.contains(bodyHash)) {
      val v: ValNum = hashTableBodies(bodyHash)
      VNBodies += (x, v)
      // remove redundant body
      Seq()
    }
    else {
      val v = x
      VNBodies += (x, v)
      hashTableBodies += (bodyHash, v)

      Seq(newBody)
    }
  }

  private def newVar(nameStr: String, ty: Option[TermType] = None, valUnkown: Boolean = false): Var = {
    val v = Var(RefByName(Name(nameStr)))
    v.typ = ty
    if valUnkown then valueUnknown = valueUnknown + v
    v
  }

  protected def simplify(term: Term): Term

  /** replaces term with Var if possible */
  override def visitTerm(term: Term): Seq[Term] = term match {
    case v@Var(RefByName(Name(name))) if const.contains(name) && this.config.propagateConstants => Seq(const(name))
    case v@Var(RefByName(Name(name))) if VN.contains(name) =>
      Seq(
        if const.contains(VN(name)) && this.config.propagateConstants then
          const(VN(name))
        else newVar(VN(name),v.typ,valueUnknown.contains(term))
      )
    case _ =>
      val newTerm = super.visitTerm(term).head
      if term.typ.nonEmpty then newTerm.typed(term.typ.get) // TODO okay ?
      val termHash: Hashed = getHashCode(newTerm)
      Seq(if (hashTable.contains(termHash)) then newVar(hashTable(termHash),term.typ,valueUnknown.contains(term)) else simplify(newTerm))
  }


  override def visitAtom(atom: Atom): Seq[Atom] = atom match{
    case Eq(vari@Var(RefByName(Name(x))), e, false) if vari.mode.isBinding =>
      treatBindingInEq(x,e, dontRemove = isParam(x), vari.typ)  // in case a redundant binding is found it will be removed unless it belongs to parameter
    case Eq(e, vari@Var(RefByName(Name(x))), false) if vari.mode.isBinding =>
      treatBindingInEq(x,e, dontRemove = isParam(x), vari.typ)
    case Eq(vari@Var(RefByName(Name(x))), e, false) =>
      treatComparisonEq(x,e, vari.typ) // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality
    case Eq(e, vari@Var(RefByName(Name(x))), false) =>
      treatComparisonEq(x,e, vari.typ)


    case call@Call(_, _, false) =>
      super.visitAtom(atom).head match{
        case call@Call(ref, args, false) => treatBindingsInCall(call,args)
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
      super.visitAtom(atom).head match{
        case call@ExtensionalCall(ref, args, false) => treatBindingsInCall(call, args)
      }


    case _ => val newAtomSeq = valueNumberAtoms(super.visitAtom(atom).head)
      removeAtomIfTrue(newAtomSeq)
  }

  protected def isConst(term: Term): Boolean = false

  private def treatBindingInEq(x: String, e: Term, dontRemove: Boolean = false, typ: Option[TermType]): Seq[Atom] = {
    val newTerm = visitTerm(e).head

    val exprHash: Hashed = getHashCode(newTerm)
    if (hashTable.contains(exprHash)) {
      val v: ValNum = newTerm match {
        case Var(RefByName(Name(str))) => str // then term was already replaced in visitTerm
        case _ => hashTable(exprHash) // term was already processed in visitTerm but there it was decided not to replace it TODO looked up twice in hashtable
      }
      VN += (x, v)

      //count = count.updated(exprHash, count(exprHash) + 1)

      // remove "Assignment" or replace term
      if dontRemove then {
        // also remember the new Eq but dont remove it
        valueNumberAtoms(Eq(newVar(x, typ, valueUnknown.contains(e)), newTerm), dontRemove = dontRemove)
      } // newTerm instead of Var(Name(v)) so that what is replaced only decided in visitTerm
      else Seq()
    }
    else if (hashTableGlobal.contains(exprHash) && !isParam(x)) {
      val v: ValNum = newTerm match {
        case Var(RefByName(Name(str))) => str // then term was already replaced in visitTerm
        case _ => hashTableGlobal(exprHash) // term was already processed in visitTerm but there it was decided not to replace it TODO looked up twice in hashtable
      }
      VN += (x, v)
      VN += (v, v)
      hashTable += (exprHash,v)

      //count = count.updated(exprHash, count(exprHash) + 1)

//      if(isParam(x) && !isParam(v)){ // when replacing a variable that is a parameter we also need to replace it in the list of parameters
//        val temp = relationParams.diff(Seq(Name(x)))
//        relationParams = temp.appended(v)
//      }

      // remove "Assignment" or replace term
      return Seq(Eq(newVar(v,typ), newTerm)) // newTerm instead of Var(Name(v)) so that what is replaced only decided in visitTerm // TODO valueNumberAtom ?
    }

    else {
      val v = x
      VN += (x, v)
      hashTable += (exprHash, v)

      //count += (exprHash,1)

      if (isConst(newTerm) && this.config.propagateConstants) {
        const += (x, newTerm)
        if !dontRemove then return Seq() // remove binding of constant -> usages of var are replaced with constant
      }

      // return with newTerm
      // also remember the new Eq; it might be removed
        valueNumberAtoms(Eq(newVar(x, typ, valueUnknown.contains(e)), newTerm), dontRemove = dontRemove)
    }
  }

  def treatComparisonEq(x: String, e: Term, typ: Option[TermType]): Seq[Atom] = {
    val atomSeq = treatBindingInEq(x, e, dontRemove = true, typ) // not removed by value numbering of terms but may be removed if duplicate of other Eq below
    if atomSeq.isEmpty then return atomSeq

    val newAtom = atomSeq.head
    val newAtomSeq = {
      if (valueUnknown.contains(Var(x))) {
        if (!valueUnknown.contains(e)) then valueUnknown = valueUnknown.removedAll(Seq(Var(x)))
        valueNumberAtoms(newAtom, dontRemove = true)
      }
      else valueNumberAtoms(super.visitAtom(newAtom).head) // dontRemove = newAtom.vars.exists(arg => isParam(arg.toString))
    }
    removeAtomIfTrue(newAtomSeq)
  }

  protected def removeAtomIfTrue(newAtomSeq: Seq[Atom]): Seq[Atom] = {
    if newAtomSeq.isEmpty || !config.removeTrueAtoms then return newAtomSeq

    newAtomSeq.head match {
      case Eq(rhs, lhs, false) if rhs == lhs => Seq()
      case _ => newAtomSeq
    }
  }

  private def treatBindingsInCall(call: Call | ExtensionalCall, args: Seq[Arg]): Seq[Atom] = {
    val atomHash: Hashed = getHashCode(call)
    val newArgs: Seq[Arg] = args.map {
      case t@TermArg(term) => term match {

        case vari@Var(RefByName(Name(variName))) if vari.mode.isBinding => // add binding vars to maps
          val bindingCallHash = getHashCode(call, Some(variName))

          // same calls except currently binding var should have same ValNum in different Relations
          if (hashTableGlobal.contains(bindingCallHash)) {
            val v: ValNum = hashTableGlobal(bindingCallHash)
            VN += (variName, v)
            hashTable += (bindingCallHash, v)
            valueUnknown = valueUnknown + Var(v)
            TermArg(newVar(v, term.typ, true))
          } else {
            VN += (variName, variName)
            hashTable += (bindingCallHash, variName)
            valueUnknown = valueUnknown + Var(variName)
            t
          }

        case _ => t
      }
      case t => t
    }
    val newCall = call match{
      case Call(ref,_,neg) => Call(ref,newArgs,neg)
      case ExtensionalCall(ref,_,neg) => ExtensionalCall(ref,newArgs,neg)
    }
    valueNumberAtoms(newCall)
  }

  private def valueNumberAtoms(atom: Atom, dontRemove: Boolean = false): Seq[Atom] = { // callArgs only given to function if atom is a call
//    val newAtom = super.visitAtom(atom).head
    val newAtom = atom
    val atomHash: Hashed = getHashCode(newAtom)
    val x = atom.toString

    if (hashTableAtoms.contains(atomHash)) {
      val v: ValNum = hashTableAtoms(atomHash)
      VNAtoms += (x, v)
      // remove Call or replace args
      if dontRemove then Seq(newAtom)
      else Seq()
    }
    else {
      val v = x
      VNAtoms += (x, v)
      hashTableAtoms += (atomHash, v)

      Seq(newAtom)
    }
  }


}
