package inca.ir.valueNumbering

import inca.ir
import inca.ir.{Atom, Call, ExtensionalCall, Name, RefByName, Term, TermArg, Var}
import inca.ir.visitors.IRVisitor

import scala.collection.mutable


type Id = Int // TODO

class Ids{ // table from term to id
  private val ids: mutable.Map[Term,Id] = mutable.Map()
  private val atomIds: mutable.Map[Atom,Id] = mutable.Map()
  private var currentId: Id = 0
  private def nextId(): Id =
    currentId += 1
    currentId
  protected def getIdOf(t: Term): Id = ids.getOrElse(t,{
    ids.update(t,nextId())
    ids(t)
  })
  // TODO needed? <- can equivalence of terms be concluded from calls?
  private def getIdOf(atom: Atom): Id = atomIds.getOrElse(atom,{
    atomIds.update(atom,nextId())
    atomIds(atom)
  })
  protected def getIdOf(atom: Atom, bindingVar: Var): Id = ids.getOrElse(bindingVar,{
    case class bindingArg() extends Term { // serves as a marker which argument is currently binding
      override def vars: Seq[Var] = Seq()
    }
    ids.update(bindingVar,
      atom match {
        case Call(ref, args, neg) =>
          val argsFiltered = args.patch(args.indexOf(TermArg(bindingVar)), Seq(TermArg(bindingArg())), 1)
          getIdOf(Call(ref, argsFiltered, neg))
        case ExtensionalCall(ref, args, neg) =>
          val argsFiltered = args.patch(args.indexOf(TermArg(bindingVar)), Seq(TermArg(bindingArg())), 1)
          getIdOf(ExtensionalCall(ref, argsFiltered, neg))
        case _ => throw new IllegalArgumentException("This should not happen")
    })
    ids(bindingVar)
  })


}

//class ValNumTable{
//  private def table: mutable.Map[Id,CongruenceClass] = mutable.Map()
//
//}

//trait BaseValueNumbering(config: ConfigVN = ConfigVN()) extends IRVisitor {
//
//  type ValNum = String
//  type Hashed = Int
//
//  var VN: Map[Name, ValNum] = Map()
//  var hashTable: Map[Hashed, ValNum] = Map()
//
//  var valueUnknown: mutable.Map[Var, mutable.Set[Var]] = mutable.Map() // remembers variables that where bound in calls -> if they are compared in Eq those shouldnt be removed
//
//  var hashFunction: mutable.Map[Term,Hashed] = mutable.Map()
//
//  def valueNumbering(module: ir.Module): ir.Module = {
//    visitModule(module)
//  }
//


//}