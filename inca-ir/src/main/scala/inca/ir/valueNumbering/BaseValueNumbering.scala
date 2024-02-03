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

  // maps for bodies
  var VNBodies: Map[String, ValNum] = Map()
  var hashTableBodies: Map[Hashed, ValNum] = Map()


  def valueNumbering(module: ir.Module): ir.Module = {
    visitModule(module)
  }

  // TODO dont use scala`s hashing function
  // Type of argument -> changed from Term to Analyzable since need to Hash Atoms like Calls too
  protected def getHashCode(elem: Analyzable): Hashed = elem match {
    case Var(RefByName(Name(name))) if VN.contains(name) => hashTable.find(_._2 == name).head._1
    case _ => elem.hashCode()
  }
  protected def getHashCode(body: Body): Hashed = body.hashCode()


  private var relationParams: Seq[Name] = Seq()
  private def isParam(x: String): Boolean = relationParams.map(_.name).contains(x)

  override def visitRelation(relation: Relation): Seq[Relation] = {
    relationParams = relation.params.map(_.name)
    VNBodies = Map()
    hashTableBodies = Map()
    super.visitRelation(relation)
  }

  override def visitBody(body: Body): Seq[Body] = {
    VN = Map()
    hashTable = Map()
    const = Map()
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

  private def newVar(nameStr: String, ty: Option[TermType] = None): Var = {
    val v = Var(RefByName(Name(nameStr)))
    v.typ = ty
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
        else newVar(VN(name),term.typ)
      )
    case _ =>
      val newTerm = super.visitTerm(term).head
      if term.typ.nonEmpty then newTerm.typed(term.typ.get) // TODO okay ?
      val termHash: Hashed = getHashCode(newTerm)
      Seq(if (hashTable.contains(termHash)) then newVar(hashTable(termHash),term.typ) else simplify(newTerm))
  }


  override def visitAtom(atom: Atom): Seq[Atom] = atom match{
    case Eq(vari@Var(RefByName(Name(x))), e, false) if vari.mode.isBinding =>   // use vari.mode.isBinding so that next case is chosen correctly
      treatBindingInEq(x,e)
    case Eq(e, vari@Var(RefByName(Name(x))), false) /*if vari.mode.isBinding*/ =>   // TODO here too?
      treatBindingInEq(x,e)
    case Eq(Var(RefByName(Name(x))), e, false) =>
      treatBindingInEq(x,e)

    case call@Call(ref, args, false) => valueNumberAtoms(call, args)
    // TODO can equivalence of two vars not be concluded from calls?
    //  could be e.g. that b(x) :- x == 0. b(x) :- x == 2. so that b(a1), b(a2) not necessarily implies that a1 == a2
    //        val otherRelation = ref.target.get
    //        if (otherRelation.bodies.size <= 1){  // then args which are bound by call have a unique value
    //          args.map{arg =>
    //           ???
    //          }
    //        }

    case _ => valueNumberAtoms(atom)
  }

  protected def isConst(term: Term): Boolean = false

  private def treatBindingInEq(x: String, e: Term): Seq[Atom] = {
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
      if isParam(x) then Seq(Eq(newVar(x,e.typ), newTerm)) // newTerm instead of Var(Name(v)) so that what is replaced only decided in visitTerm
      else Seq()
    }
    else {
      val v = x
      VN += (x, v)
      hashTable += (exprHash, v)

      //count += (exprHash,1)

      if (isConst(newTerm) && this.config.propagateConstants) {
        const += (x, newTerm)
        if !isParam(x) then return Seq() // remove binding of constant -> usages of var are replaced with constant
      }

      Seq(
        // return with newTerm
        Eq(newVar(x,e.typ), newTerm)
      )
    }
  }


  private def valueNumberAtoms(atom: Atom, callArgs: Seq[Arg] = Seq()): Seq[Atom] = { // callArgs only given to function if atom is a call
    val newAtom = super.visitAtom(atom).head
    val atomHash: Hashed = getHashCode(newAtom)
    val x = atom.toString

    if (hashTable.contains(atomHash)) { // TODO other Maps for Atoms or okay like this ?
      val v: ValNum = hashTable(atomHash)
      VN += (x, v)
      // remove Call or replace args
      if atom.vars.exists(arg => isParam(arg.toString)) then Seq(newAtom)
      else Seq()
    }
    else {
      val v = x
      VN += (x, v)
      hashTable += (atomHash, v)

      callArgs.foreach { case TermArg(t) => t match // add binding vars to maps
        case vari@Var(RefByName(Name(variName))) if vari.mode.isBinding => VN += (variName, variName); hashTable += (atomHash, variName)//; count += (atomHash, 1)
//        case Var(RefByName(Name(variName))) =>
//          VN(variName)
//          count = count.updated(atomHash, count(atomHash) + 1)
        case _ =>
      }

      Seq(newAtom)
    }
  }


}
