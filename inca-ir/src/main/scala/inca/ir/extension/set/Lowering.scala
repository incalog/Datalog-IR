package inca.ir.extension.set

/* I'll briefly sketch two different, but similar approaches of how first-class sets
 * (without the empty set) could be handled:
 *
 * Proposal 1: Represent set with IDs expressed as ADTs
 * E.g
 * main(z: Set[Int]) :- y == Set(1,2,3), somCall(y, z).
 * someCall(y: Set[Int], z: Set[Int]) :- z == (y U Set(2,4))
 *
 * Lowering:
 * SetADT = Set$0 | Set$1 | Set$2(y)
 *
 * set(Set$0, x: Int) :- #prefix, (x == 1 v x == 2 v x == 3)
 * set(Set$1, x: Int) :- #prefix, (x == 2 v x == 4)
 *
 * set(Set$2(y), x: Int) :- #prefix (with y), (set(y, x) v set(Set$1, x))
 *
 * main(z: Int) :- y == Set$0, somCall(y, z).
 * someCall(y: SetADT, z: Int) :- set(Set$2(y), z).
 *
 * We replace a Set with an ID that we represent by an ADT. Instead of passing around
 * a set, we pass around an ADT value. We create one (or multiple) set relation that
 * include all atoms up to this point as a prefix. When we read a set, we query this
 * set relation with the ID we generated for the set.
 *
 * Problem: Equality between sets might not work, since sets might have the same
 * Elements, but different ADT representations.
 *
 *
 * Proposal 2: Represent set with their Values
 * E.g
 *
 * main(z: Set[Int]) :- y == Set(1,2,3), somCall(y, z).
 * someCall(y: Set[Int], z: Set[Int]) :- z == (y U Set(2,4))
 *
 * Lowering:
 * set$0(x: Int) :- (x == 1 v x == 2 v x == 3)
 * set$1(x: Int) :- (x == 2 v x == 4)
 *
 * // All bound variables we need to construct the set are inputs to the relation
 * set$2(x: Int, z: Int, y: Int) :- (y == x v y == z)
 *
 * main(z: Int) :- set$0(tmp), y == tmp, someCall(y, z)
 * someCall(y: Int, z: Int) :- set$1(tmp), set$2(y, tmp, z).
 *
 * Instead of representing Set with ADT Ids, we pass around the values of a set directly.
 * I don't think we need a prefix here, since we just pass the concrete set value to the helper
 * relations.
 * Disadvantage: If the set contains tuple of size n we might end up passing around n values.
 * I'm still not sure if there are cases where this does not work...
 *
 * Problem: Eq / Neq does not work anymore
 * Before: Set(1,2) == Set(2,4) // False
 * After: 1 == 2 v 1 == 4 v 2 == 2 v 2 == 4 // One body is executed although no body should be executed
 */

import inca.ir
import inca.ir.extension.disjunction.Disjunction
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Call, Eq, Name, Neq, Relation, Term, Type, Var, Param, Body, string2name}
import inca.ir.extension.set.Set
import inca.ir.extension.disjunction
import inca.ir.extension.block

import scala.collection.immutable

object Lowering:
  def apply[S <: IR, T <: BaseIR with disjunction.IR with block.IR](srcIR: S, trgIR: T): Lowering[S, T] = new Lowering[S, T] {
    override def src: S = srcIR
    override def trg: T = trgIR
  }

trait Lowering[S <: IR, T <: BaseIR with disjunction.IR with block.IR] extends BaseLowering[S, T]:
  override def loweredIRs: immutable.Set[BaseIR] = super.loweredIRs ++ immutable.Set(IR)

  override def addedIRs: Predef.Set[BaseIR] = super.addedIRs ++ immutable.Set(disjunction.IR, block.IR)

  var setRelations: Seq[Relation] = Seq()

  private def freshSetRelation(dependencies: Seq[Var], args: Seq[Term], outTyp: Type): (Var, Relation) = gensym.scoped {
    val dependentParams = dependencies.map { v =>
      Param(v.name, v.typ.getOrElse(throw IllegalStateException(s"Untyped var $v")))
    }
    val outName = gensym.fresh("return")
    val outTy = outTyp match {
      case TSet(_) => outTyp
      case ty => throw IllegalStateException(s"Expected Set, but got $ty")
    }
    val outVar = Var(outName)
    val outParam = Param(outName, visitType(outTyp))
    val rel = Relation(
      gensym.fresh("set"),
      dependentParams :+ outParam,
      Seq(Body(Seq(Disjunction(args.map { t =>
        visitTerm(t).map(tt => Eq(outVar, tt))
      }))))
    )
    (outVar, rel)
  }

  private def visitSetCompare(lhs: Term, rhs: Term): Unit = (lhs.typ, rhs.typ) match
    // TODO: We need to check if a variable is bound
    //case (None, _) => throw IllegalStateException(s"Untyped expression $lhs")
    //case (_, None) => throw IllegalStateException(s"Untyped expression $rhs")
    //case (Some(TSet(_)), Some(TSet(_))) => throw IllegalStateException("Can not compare sets for equality!")
    // TODO: We could let the body fail here
    //case (Some(TSet(_)), _) | (_, Some(TSet(_))) => throw IllegalStateException("Can not compare sets for equality!")
    case _ => // nothing

  override def visitRelation(relation: Relation): Seq[Relation] =
    super.visitRelation(relation) ++ setRelations

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Eq(lhs, rhs) =>
      visitSetCompare(lhs, rhs)
      super.visitAtom(atom)
    case Neq(lhs, rhs) =>
      visitSetCompare(lhs, rhs)
      super.visitAtom(atom)
    case SetMember(t1, t2) =>
      visitTerm(t1).zip(visitTerm(t2)).map(Eq.apply)
    case _ => super.visitAtom(atom)

  override def visitTerm(term: Term): Seq[Term] = term match
    case Set(ts) =>
      val dependentVars = term.vars
      val ty = term.typ.getOrElse(throw IllegalStateException(s"Untyped expression $term"))
      val (outVar, relation) = freshSetRelation(dependentVars, ts, ty)
      setRelations :+= relation
      Seq(block.Block(
        Seq(Call(relation.name, dependentVars :+ outVar)),
        outVar
      ))
    case SetIntersection(_, _) => ???
    case SetUnion(_, _) => ???
    case _ => super.visitTerm(term)

  override def visitType(ty: Type): Type = ty match
    case TSet(ty) => ty
    case _ => super.visitType(ty)