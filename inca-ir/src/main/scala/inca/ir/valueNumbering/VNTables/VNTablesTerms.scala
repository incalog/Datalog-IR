package inca.ir.valueNumbering.VNTables

import inca.ir.{Name, RefByName, Term, TermType, Var}
import inca.ir.extension.data.Construct



trait VNTablesTerms extends VNTablesTrait[Term] {

  override val congrClasses: CongrClassesTable[Term] = CongrClassesTable[Term]()

  override protected val newCongrClass: (ValueId, Term) => CongruenceClassTerms


  override def getReplacement(t: Term): Term = {
    if (!congrClasses.contains(valueNumbers(t))) return t

    val leader = getCongrClassOf(t).leader
    leader match {
      case vari@Var(_) => VNTablesTerms.newVar(vari.name, t.typ)
      case _ => leader
    }
  }

  override protected def getCongrClassOf(t: Term): CongruenceClassTerms = super.getCongrClassOf(t).asInstanceOf[CongruenceClassTerms]

  override def getCongrClasses: CongrClassesTable[Term] = congrClasses

  override def getCongrClassOf(id: ValueId): CongruenceClassTerms = super.getCongrClassOf(id).asInstanceOf[CongruenceClassTerms]

  def getDefiningTerm(t: Term): Term = {
    if (valueNumbers.contains(t)) {
      if (congrClasses.contains(valueNumbers(t))) {
        return getCongrClassOf(t).definingTerm
      }
    }
    return t
  }

  def getDefiningTerm(id: ValueId): Term = getCongrClassOf(id).definingTerm

  def updateCongrClassIfNecessary(vn: ValueId, t: Term, updateDefTermIfNecessary: Boolean = false): Boolean =
    getCongrClassOf(vn).updateCongrClassIfNecessary(t, updateDefTermIfNecessary)

  override def updateValueNumbersAndCongrClasses(fromId: ValueId, toId: ValueId): Boolean = {
    var isValid = true
    var updateToCongrClass = false
    if (congrClasses.contains(fromId) && congrClasses.contains(toId)) {
      isValid &= congrClasses(toId).changeLeaderIfNecessary(congrClasses(fromId).leader)
      getCongrClassOf(toId).changeDefTermIfNecessary(getDefiningTerm(fromId))
    }
    else if (congrClasses.contains(fromId) && !congrClasses.contains(toId)) {
      val congrCls = newCongrClass(toId, congrClasses(fromId).leader)
      congrCls.definingTerm = getDefiningTerm(fromId)
      congrClasses.update(toId, congrCls)
      valueNumbers.getAllWithId(toId).foreach(t => isValid &= updateCongrClassIfNecessary(toId, t))
    }
    else if (!congrClasses.contains(fromId) && congrClasses.contains(toId)) {
      updateToCongrClass = true
    }
    valueNumbers.getAllWithId(fromId).foreach(t =>
      valueNumbers.update(t, toId)
      if (updateToCongrClass) {
        isValid &= updateCongrClassIfNecessary(toId, t)
      }
    )
    congrClasses.remove(fromId)
    return isValid
  }

  
  def getConstruct(id: ValueId): Option[Construct] = this.valueNumbers.getAllWithId(id).find {
    case _: Construct => true
    case _ => false
  }.asInstanceOf[Option[Construct]]

}




object VNTablesTerms {

  def newVar(name: Name, ty: Option[TermType] = None): Var = {
    val v = Var(RefByName(name))
    v.typ = ty
    v
  }

}