package inca.ir.valueNumbering

import inca.ir
import inca.ir.*
import inca.ir.analysis.Analyzable
import inca.ir.extension.aggregate.Aggregate
import inca.util.Gensym

import scala.collection.mutable
import inca.ir.util.SourceLocation
import inca.ir.visitors.IRVisitor


/** wraps parameters for value numbering */
case class ConfigVN(simplifyArithmetic: Boolean = false,
                    propagateConstants: Boolean = false,
                    removeTrueAtoms: Boolean = false,
//                    occurrencesBeforeRemoved: Int = 0,
                    attemptAlphaEquivalence: Boolean = false,
                    outline: Boolean = false,
                    occurrencesBeforeOutlined: Int = 1,
                    minSizeOutline: Int = 2
                   )

/** for value numbering constructs from BaseIR */
//class ValueNumbering(analysis: IRAbstractInterpreter) extends IROptimizer(analysis) {
trait BaseValueNumbering(config: ConfigVN = ConfigVN()) extends IRVisitor {

  type ValNum = String
  type Hashed = Int

  // maps for terms
  var VN: Map[String, ValNum] = Map() // String is a Name TODO Map[Name, ValNum] ?
  var hashTable: Map[Hashed, ValNum] = Map()
  var const: Map[String, Term] = Map() // remembers constant term assigned to Var with name string
//  var count: Map[Hashed, Int] = Map()   // remembers how often term with hash has occurred

  var valueUnknown: Set[Term] = Set() // remembers variables that where bound in calls -> if they are compared in Eq those shouldnt be removed

  // maps for atoms
  var hashTableAtoms: Map[Hashed, (Atom,Int)] = Map()

  // maps for bodies
  var hashTableBodies: Map[Hashed, ValNum] = Map()

  // maps for relations
  var VNRelations: Map[String, ValNum] = Map()
  var hashTableRelations: Map[Hashed, ValNum] = Map()

  var gensym: Gensym = Gensym()
  var substParamNames: Seq[String] = Seq()

  // global Maps for terms
  var VNGlobal: Map[String, ValNum] = Map() // String is a Name
  var hashTableGlobal: Map[Hashed, ValNum] = Map()

  // global Maps for atoms:  remembers info about atom with hash
  var hashTableAtomsGlobal: mutable.Map[Hashed, mutable.Seq[AtomInfo]] = mutable.Map() // TODO use less space ?


  def valueNumbering(module: ir.Module): ir.Module = {
    // ugly fix for changing references from removed relations  // TODO refactor / rewrite so that relations are processed in different order
    class CallsFix extends IRVisitor {
      def fixCalls(module: Module): Module = {
        super.visitModule(module)
      }
      override def visitAtom(atom: Atom): Seq[Atom] = atom match {
        case Call(ref, args, neg) => Seq(Call(VNRelations(ref.name.name), args, neg))
        case Aggregate(rel, args, op) => Seq(Aggregate(RefByName(Name(VNRelations(rel.name.name))),args,op)) // TODO move in separate aggregate file
//        case ExtensionalCall(ref, args, neg) => Seq(ExtensionalCall(VNRelations(ref.name.name), args, neg))
        case _ => Seq(atom) // no further descend in AST
      }
    }

    val newModule = visitModule(module)
    val fixed = new CallsFix().fixCalls(newModule)
    if config.outline then return new Outlining().outlineCommonAtoms(fixed) else return fixed

  }

  override def visitModule(module: Module): Module = {
    VNRelations = Map()
    hashTableRelations = Map()

    if (config.attemptAlphaEquivalence){
      gensym = Gensym(module.relations.values.flatMap(rel => rel.params.map(_.name.name).concat(rel.bodies.flatMap(_.vars.map(_.name.name)))))
      val maxNumParams = module.relations.values.map(_.params.length).max
      (0 until maxNumParams).foreach( _ => substParamNames = substParamNames.appended(gensym.fresh("param")))
    }

    super.visitModule(module)
  }


  // TODO dont use scala`s hashing function
  protected def getHashCode(elem: Term): Hashed = elem match {
    case Var(RefByName(Name(name))) if VN.contains(name) && hashTable.exists(_._2 == name) => hashTable.find(_._2 == name).head._1
    case _ => elem.hashCode()
  }

  // bindingArg is passed if hash is used for Var with this name and NOT for the atom
  protected def getHashCode(atom: Atom, bindigArg: Option[String] = None): Hashed = atom match {
    case Call(ref,args,neg) => bindigArg match {
      case Some(withoutArg) =>
        val argsFiltered = args.filterNot { // there should be no false positive caused by removal because no relation name used twice
          case TermArg(Var(RefByName(Name(x)))) => x == withoutArg
          case _ => false
        }
        (Seq(ref) ++ argsFiltered ++ Seq(neg)).hashCode()
      case None => (Seq(ref) ++ args ++ Seq(neg)).hashCode()
    }
    case ExtensionalCall(ref,args,neg) => bindigArg match {
      case Some(withoutArg) =>
        val argsFiltered = args.filterNot { // there should be no false positive caused by removal because no relation name used twice
          case TermArg(Var(RefByName(Name(x)))) => x == withoutArg
          case _ => false
        }
        (Seq(ref) ++ argsFiltered ++ Seq(neg)).hashCode()
      case None => (Seq(ref) ++ args ++ Seq(neg)).hashCode()
    }
    case _ => atom.hashCode()
  }

  protected def getHashCode(body: Body): Hashed = body.hashCode()

  protected def getHashCode(relation: Relation): Hashed = (relation.params ++ relation.bodies).hashCode()  // -> name of relation irrelevant


  private var currentRelationName: Name = _

  private var relationParams: Seq[Name] = Seq()
  private def isParam(x: String): Boolean = relationParams.map(_.name).contains(x)

  private var paramSubst: Map[String,String] = Map()

  private var currentBodyIndex: Int = -1

  override def visitRelation(relation: Relation): Seq[Relation] = {
    currentRelationName = relation.name
    relationParams = relation.params.map(_.name)
    hashTableBodies = Map()

    if (config.attemptAlphaEquivalence) { // TODO include?  and if save correct relationParams
      paramSubst = Map()
      relationParams.zip(substParamNames).foreach(paramSubst += (_, _))
    }

    currentBodyIndex = -1
    valueNumberRelations(relation)
  }

  override def visitParam(param: Param): Seq[Param] = {
    if paramSubst.contains(param.name.name) then Seq(Param(paramSubst(param.name.name), param.ty))
    else Seq(param)
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


  private var currentBodyVars: Seq[Var] = Seq()
  private def isUsedInBody(x: String): Boolean = currentBodyVars.map(_.name.name).contains(x) // TODO like this or use gensym?

  private var currentAtomIndex: Int = -1   // TODO prettier solution ?

  override def visitBody(body: Body): Seq[Body] = {
    VNGlobal = VNGlobal ++ VN
    hashTableGlobal = hashTableGlobal ++ hashTable

    VN = Map()
    hashTable = Map()
    const = Map()

    hashTableAtoms = Map()
    currentAtomIndex = -1
    currentBodyIndex += 1
    currentBodyVars = body.vars
    val newBodySeq = valueNumberBodies(body)
    if newBodySeq.nonEmpty then
      hashTableAtoms.foreach {
        case (hash, (atom, idx)) => hashTableAtomsGlobal.updateWith(hash) {
          _ => Some(hashTableAtomsGlobal.getOrElse(hash, mutable.Seq()).appended(AtomInfo(atom, currentRelationName, currentBodyIndex, idx)))
        }
      }
    newBodySeq
  }

  private def valueNumberBodies(body: Body): Seq[Body] = {
    val newBody = super.visitBody(body).head

    // remove if redundant
    val bodyHash: Hashed = getHashCode(newBody)
    if (hashTableBodies.contains(bodyHash)) {
      Seq()
    }
    else {
      val x = newBody.toString // TODO save space
      hashTableBodies += (bodyHash, x)
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
    case v@Var(RefByName(Name(name))) if paramSubst.contains(name) && config.attemptAlphaEquivalence => visitTerm(newVar(paramSubst(name),v.typ,valueUnknown.contains(term)))
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


  override def visitAtom(atom: Atom): Seq[Atom] = atom match {
    case Eq(vari@Var(RefByName(Name(x))), e, false) if vari.mode.isBinding =>
      treatBindingInEq(x, e, dontRemove = isParam(x), vari.typ) // in case a redundant binding is found it will be removed unless it belongs to parameter
    case Eq(e, vari@Var(RefByName(Name(x))), false) if vari.mode.isBinding =>
      treatBindingInEq(x, e, dontRemove = isParam(x), vari.typ)
    case Eq(vari@Var(RefByName(Name(x))), e, false) =>
      treatComparisonEq(x, e, vari.typ) // not removed since non binding Eq is comparison that might reduce number of solutions; but remember equality
    case Eq(e, vari@Var(RefByName(Name(x))), false) =>
      treatComparisonEq(x, e, vari.typ)


    case call@Call(_, _, false) =>
      super.visitAtom(atom).head match {
        case call@Call(ref, args, false) => treatBindingsInCall(call, args)
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

    case _ => val newAtomSeq = valueNumberAtoms(super.visitAtom(atom).head)
      removeAtomIfTrue(newAtomSeq)
  }

  protected def isConst(term: Term): Boolean = false

  private def treatBindingInEq(varName: String, e: Term, dontRemove: Boolean = false, typ: Option[TermType]): Seq[Atom] = {
    val newEqSeq = valueNumberingTerm(varName,e,dontRemove,typ)
    if newEqSeq.isEmpty then return Seq()

    val newEq = newEqSeq.head
    valueNumberAtoms(newEq,dontRemove=dontRemove)
  }

  private def treatComparisonEq(x: String, t: Term, typ: Option[TermType]): Seq[Atom] = {
    val newEqSeq = valueNumberingTerm(x, t, dontRemove = true, typ)
    if newEqSeq.isEmpty then return Seq()

    val newEq = newEqSeq.head
    val newAtomSeq = {
      if (valueUnknown.contains(Var(x))) {
        if (!valueUnknown.contains(t)) then valueUnknown = valueUnknown.removedAll(Seq(Var(x)))
        valueNumberAtoms(newEq, dontRemove = true)
      }
      else valueNumberAtoms(super.visitAtom(newEq).head) // allowed to remove comparison since value of variable is known before -> wont reduce set of results
    }
    removeAtomIfTrue(newAtomSeq)
  }

  private def valueNumberingTerm(varName: String, t: Term, dontRemove: Boolean = false, typ: Option[TermType]): Seq[Eq] = {
    val newTerm = visitTerm(t).head
    val x = if paramSubst.contains(varName) && config.attemptAlphaEquivalence then paramSubst(varName) else varName

    val termHash: Hashed = getHashCode(newTerm)
    if (hashTable.contains(termHash)) {
      val v: ValNum = newTerm match {
        case Var(RefByName(Name(str))) => str // then term was already replaced in visitTerm
        case _ => hashTable(termHash) // term was already processed in visitTerm but there it was decided not to replace it TODO looked up twice in hashtable
      }
      VN += (x, v)

      //count = count.updated(termHash, count (termHash) + 1)

      // remove "Assignment" or replace term
      if dontRemove then {
        // newTerm instead of Var(Name(v)) so that what is replaced only decided in visitTerm
        // also remember the new Eq but dont remove it
        Seq( Eq(newVar(x, typ, valueUnknown.contains(t)), newTerm) )
      }
      else Seq()
    }
    else if (hashTableGlobal.contains(termHash) && !dontRemove) {
      val v: ValNum = newTerm match {
        case Var(RefByName(Name(str))) => str // then term was already replaced in visitTerm
        case _ => hashTableGlobal(termHash) // term was already processed in visitTerm but there it was decided not to replace it TODO looked up twice in hashtable
      }
      if (!isUsedInBody(v)) {
        VN += (x, v)
        VN += (v, v)
        hashTable += (termHash, v)

        // replace term
        return Seq( Eq(newVar(v, typ), newTerm) )  // newTerm instead of Var(Name(v)) so that what is replaced only decided in visitTerm
      }
      else{ // TODO refactor
        val v = x
        VN += (x, v)
        hashTable += (termHash, v)
        return Seq( Eq(newVar(v, typ), newTerm) )
      }
    }

    else {
      val v = x
      VN += (x, v)
      hashTable += (termHash, v)

      //count += (termHash,1)

      if (isConst(newTerm) && this.config.propagateConstants) {
        const += (x, newTerm)
        if !dontRemove then return Seq() // remove binding of constant -> usages of var are replaced with constant
      }

      // return with newTerm
      // also remember the new Eq; it might be removed
      Seq( Eq(newVar(x, typ, valueUnknown.contains(t)), newTerm) )
    }
  }

//  private def rememberEquality(x: String, v: ValNum, hash: Option[Hashed] = None): Unit = {
//    VN += (x,v)
//    if (hash.isDefined) {
//      hashTable += (hash.get, v)
//    }
//  }



  protected def removeAtomIfTrue(newAtomSeq: Seq[Atom]): Seq[Atom] = {
    if newAtomSeq.isEmpty || !config.removeTrueAtoms then return newAtomSeq

    newAtomSeq.head match {
      case Eq(rhs, lhs, false) if rhs == lhs => Seq()
      case _ => newAtomSeq
    }
  }

  private def treatBindingsInCall(call: Call | ExtensionalCall, args: Seq[Arg]): Seq[Atom] = {
    val newArgs: Seq[Arg] = args.map {
      case t@TermArg(term) => term match {

        case vari@Var(RefByName(Name(variName))) if vari.mode.isBinding => // add binding vars to maps
          val bindingCallHash = getHashCode(call, Some(variName))   // TODO is this okay?

          // same calls except currently binding var should have same ValNum in different Relations
          if (hashTableGlobal.contains(bindingCallHash) && !isParam(variName)) {
            val v: ValNum = hashTableGlobal(bindingCallHash)
            if (!isUsedInBody(v)) {
              VN += (variName, v)
              hashTable += (bindingCallHash, v)
              valueUnknown = valueUnknown + Var(v)
              TermArg(newVar(v, term.typ, true))
            }
            else {
              VN += (variName, variName)
              hashTable += (bindingCallHash, variName)
              valueUnknown = valueUnknown + Var(variName)
              t
            }
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

  private def valueNumberAtoms(atom: Atom, dontRemove: Boolean = false): Seq[Atom] = {
    // remove if redundant
    val atomHash: Hashed = getHashCode(atom)
    if (hashTableAtoms.contains(atomHash)) {
      if dontRemove then Seq(atom)
      else Seq()
    }
    else {
      currentAtomIndex += 1
      hashTableAtoms += (atomHash, (atom,currentAtomIndex))
      Seq(atom)
    }
  }




  case class AtomInfo(atom: Atom, name: Name, bodyIdx: Int, atomIdx: Int) {
    override def toString: String = s"$atom | ${name.name} | $bodyIdx | $atomIdx"

  }
  class Outlining{ // TODO refactor (?)

    type candidateBody = Seq[AtomInfo]

    def outlineCommonAtoms(module: Module): Module = {
      val atomsPerBody = preprocessCollectedData()

      val newBodiesWithInfosAboutOriginalAtoms: Seq[(Body,Seq[AtomInfo])] = findCommonAtomsInARow(atomsPerBody)

      val usedRelNames = module.relations.keys
      gensym.register(usedRelNames.map(_.name))

      val newRelations = updateRelations(module.relations,newBodiesWithInfosAboutOriginalAtoms)
      val otherEntries = module.entries.filterNot(_._2.isInstanceOf[Relation])
      Module(module.name, module.lang, newRelations ++ otherEntries.values)
    }

    private def preprocessCollectedData(): Seq[Seq[AtomInfo]] = {
      val commonAtoms: Seq[mutable.Seq[AtomInfo]] =
        // makes sure that no atoms -> bodies -> relations are looked at that are known to not contain relevant duplicates
        hashTableAtomsGlobal.values.filter(_.length > config.occurrencesBeforeOutlined).toSeq
      val candidateAtoms: Seq[AtomInfo] = commonAtoms.flatten
      val atomsPerBody: Seq[Seq[AtomInfo]] = candidateAtoms.groupBy(aI => (aI.name, aI.bodyIdx)).toSeq.map(_._2.sortWith((l, r) => l.atomIdx <= r.atomIdx))
      atomsPerBody
    }

    private def findCommonAtomsInARow(atomsPerBody: Seq[Seq[AtomInfo]]): Seq[(Body,Seq[AtomInfo])] = {
      if atomsPerBody.isEmpty then return Seq()

      def noIndexJumps(candidate: candidateBody): Boolean = { // TODO refactor...
        // make sure there is no jump in indices between atoms in original body
        val idc = candidate.map(_.atomIdx)
        var prev = idc(0)
        (1 until idc.size).map{ i =>
          val current = idc(i)
          val result = current - prev == 1
          prev = current
          result
        }.forall(_==true)
      }

      var bodiesTable: Seq[(Body,Seq[AtomInfo])] = Seq()
      val maxSize = atomsPerBody.map(_.size).max

      (config.minSizeOutline to maxSize).foreach { k =>
        val hashtable: mutable.Map[Hashed, mutable.Seq[candidateBody]] = mutable.Map()
        val candidatesK: Seq[candidateBody] = atomsPerBody.flatMap(_.grouped(k).toSeq).filter(candidate => candidate.size == k && noIndexJumps(candidate))
        /* determine max size clones
         *   1. place current subset in hashtable
         *   2. check whether enough occurrences in different bodies are equal / in same bucket
         *   3. if then save subset in table
         *   4. and remove smaller subsets that are contained in current
         *   5. -> table should contain the cloned subsets with maximum size
         * */
        val candidateBodiesK: Seq[Body] = candidatesK.map(cand =>
          val candBody = Body(cand.map{ case AtomInfo(atom, name, bodyIdx, atomIdx) => atom })
          val hash = candBody.hashCode()
          hashtable.updateWith(hash) {
          _ => Some(hashtable.getOrElse(hash, mutable.Seq()).appended(cand))
          }
          candBody
        )
        val candidateEntries = hashtable.values
        val filteredCandidates = candidateEntries.filter(_.size > config.occurrencesBeforeOutlined)
        filteredCandidates.foreach { candidateSeq =>
          val candidate = Body(candidateSeq.head.map{case AtomInfo(atom, _, _, _) => atom})
          bodiesTable = bodiesTable.filter{case (candBody,atomInfoSeq) => candidate.atoms.intersect(candBody.atoms).isEmpty } // remove smaller clones
          bodiesTable = bodiesTable.appended((candidate, candidateSeq.flatten.toSeq)) // need to remember all the original locations to change them in the next step
        }
      }
        // TODO how to make sure that there is no "Ill-typed equation"
        //  i.e. handle cases in which binding of a Var is different (-> not in new body)
        bodiesTable
    }


    private def getNeededParameters(body: Body): Seq[(Param,TermType)] =
      body.atoms.flatMap(_.vars).distinct.map(v => (Param(v.name, v.typ.get.ty), v.typ.get)) // TODO to many vars ?

    private def updateRelations(oldRelations: Map[String, Relation], newBodiesWithInfosAboutOriginalAtoms: Seq[(Body, Seq[AtomInfo])]): Seq[Relation] = {
      var relations = oldRelations

      val newRelations = newBodiesWithInfosAboutOriginalAtoms.flatMap { case (newBody, atomInfos) =>
        // generate new relation
        val paramsWithTypes = getNeededParameters(newBody)
        val (params, _) = paramsWithTypes.unzip
        val newRel = generateNewRelation(params, newBody)

        // insert call into existing relations
        val newCall = generateNewCall(newRel, paramsWithTypes)
        val relBodiesToChange = atomInfos.groupBy { case AtomInfo(_, name, bodyIdx, atomIdx) => (name, bodyIdx) }
        relBodiesToChange.foreach { case ((name, bodyIdx), atomInfoSeq) =>
          val changedRel = changeRelation(relations(name), bodyIdx, atomInfoSeq.map(_.atom), newCall)
          relations = relations.updated(changedRel.name, changedRel)
          changedRel
        }

        Seq(newRel)
      }
      newRelations ++ relations.values
    }

    private def generateNewRelation(params: Seq[Param], body: Body): Relation = {
      val name = gensym.freshName("R")
      Relation(name, params, Seq(body))
    }

    private def generateNewCall(relation: Relation, parametersWithType: Seq[(Param,TermType)]): Call = {
      val args: Seq[Arg] = parametersWithType.map{ case (Param(name, ty), termType) => TermArg(newVar(name, Some(termType)))} // TODO mode of type correct ?
      Call(relation.name, args)
    }

    private def changeRelation(rel: Relation, bodyIdx: Int, removed: Seq[Atom], newCall: Call): Relation = {
      val newBody = removeOutlinedAtomsAndInsertCall(rel.bodies(bodyIdx), removed, newCall)
      val newBodies = rel.bodies.patch(bodyIdx, Seq(newBody), 1)
      Relation(rel.name,rel.params,newBodies)
    }

    private def removeOutlinedAtomsAndInsertCall(oldBody: Body, removed: Seq[Atom], call: Call): Body = {
      var newAtoms: Seq[Atom] = Seq()
      var inserted = false
      (0 until oldBody.atoms.size).foreach { i =>
        val atom = oldBody.atoms(i)
        if(!removed.contains(atom)) { // TODO does that always work?
          newAtoms = newAtoms.appended(atom)
        }
        else if (!inserted) {
          newAtoms = newAtoms.appended(call)
          inserted = true
        }
      }
      Body(newAtoms)
    }


  }


}
