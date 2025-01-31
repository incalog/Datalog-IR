package inca.ir.valueNumbering.VNTables

import inca.ir.{Name, RefByName, Term, TermType, Var}
import inca.ir.extension.data.Construct



trait VNTablesTerms extends VNTablesTrait[Term] {

  override val congrClasses: CongrClassesTable[Term] = CongrClassesTable[Term]()

  override protected val newCongrClass: (ValueNumber, Term) => CongruenceClassTerms


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

  override def getCongrClassOf(valNum: ValueNumber): CongruenceClassTerms = super.getCongrClassOf(valNum).asInstanceOf[CongruenceClassTerms]

  def getDefiningTerm(t: Term): Term = {
    if (valueNumbers.contains(t)) {
      if (congrClasses.contains(valueNumbers(t))) {
        return getCongrClassOf(t).definingTerm
      }
    }
    return t
  }

  def getDefiningTerm(valNum: ValueNumber): Term = getCongrClassOf(valNum).definingTerm

  def updateCongrClassIfNecessary(vn: ValueNumber, t: Term, updateDefTermIfNecessary: Boolean = false): Boolean =
    getCongrClassOf(vn).updateCongrClassIfNecessary(t, updateDefTermIfNecessary)

  override def updateValueNumbersAndCongrClasses(fromVn: ValueNumber, toVn: ValueNumber): Boolean = {
    var isValid = true
    var updateToCongrClass = false
    if (congrClasses.contains(fromVn) && congrClasses.contains(toVn)) {
      isValid &= congrClasses(toVn).changeLeaderIfNecessary(congrClasses(fromVn).leader)
      getCongrClassOf(toVn).changeDefTermIfNecessary(getDefiningTerm(fromVn))
    }
    else if (congrClasses.contains(fromVn) && !congrClasses.contains(toVn)) {
      val congrCls = newCongrClass(toVn, congrClasses(fromVn).leader)
      congrCls.definingTerm = getDefiningTerm(fromVn)
      congrClasses.update(toVn, congrCls)
      valueNumbers.getAllWithVn(toVn).foreach(t => isValid &= updateCongrClassIfNecessary(toVn, t))
    }
    else if (!congrClasses.contains(fromVn) && congrClasses.contains(toVn)) {
      updateToCongrClass = true
    }
    valueNumbers.getAllWithVn(fromVn).foreach(t =>
      valueNumbers.update(t, toVn)
      if (updateToCongrClass) {
        isValid &= updateCongrClassIfNecessary(toVn, t)
      }
    )
    congrClasses.remove(fromVn)
    return isValid
  }

  
  def getConstruct(vn: ValueNumber): Option[Construct] = this.valueNumbers.getAllWithVn(vn).find {
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