package inca.ir.extension.set

/* I'll briefly sketch two different, but similar approaches of how first-class sets
 * (without the empty set) could be handled. Both approaches have their problems that I
 * still need to figure out:
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
 * // We should actually not defunctionalize Set(2,4) here.
 * set(Set$2(y), x: Int) :- #prefix (with y), (set(y, x) v set(Set$1, x))
 *
 * main(z: Int) :- y == Set$0, somCall(y, z).
 * someCall(y: SetADT, z: Int) :- set(Set$2(y), z).
 *
 * We replace a Set with an ID that we represent by an ADT. Instead of passing around
 * a set, we pass around an ADT value. The ADT case must include all variables bound
 * in the Set expression. We create one (or multiple) set relation that include all
 * atoms up to this point as a prefix. When we read a set, we query this set relation
 * with the ID we generated for the set.
 *
 * Problem:
 * 1. Equality will not work:
 * E.g y == 1, x == Set(y, 2), x == Set(1,2)
 * This will generate two different ADT cases: Set$0(y) and Set$1. These cases are
 * different although the elements are the same. We could implement equality by aggregation
 *
 * 2. How do we know when we can use "real" set relations instead of ADT values ?
 * We don't know when a value is a return value. Do we just let the user handle this
 * manually ? We do now it for set union call and the like. Do we require adornment information ?
 * Is adornment information enough ?
 *
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
 *
 * Problem 2: Aggregate
 * How do we aggregate over a passed to a program as variable ? We can't.
 * E.g x = Set(1,2,3), aggregate(x, some_aggregation)
 * We don't know by which relation x is represented !
 */

import inca.ir
import inca.ir.extension.disjunction.Disjunction
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Call, Eq, ModuleEntry, Name, Neq, Param, Relation, Term, Type, Var, string2name}
import inca.ir.extension.set.Set
import inca.ir.extension.disjunction
import inca.ir.extension.block
import inca.ir.extension.data
import inca.ir.extension.data.{CaseDefinition, Construct, DataDefinition, TData}

import scala.collection.immutable

object Lowering:
  def apply[S <: IR, T <: BaseIR with disjunction.IR with block.IR with data.IR](srcIR: S, trgIR: T): Lowering[S, T] = new Lowering[S, T] {
    override def src: S = srcIR
    override def trg: T = trgIR
  }

// This implements Proposal 1:
trait Lowering[S <: IR, T <: BaseIR with disjunction.IR with block.IR with data.IR] extends BaseLowering[S, T]:
  override def loweredIRs: immutable.Set[BaseIR] = super.loweredIRs ++ immutable.Set(IR)

  override def addedIRs: Predef.Set[BaseIR] = super.addedIRs ++ immutable.Set(disjunction.IR, block.IR, data.IR)

  var setDefunRelations: Seq[Relation] = Seq()
  var setDefunType: Option[Type] = None
  var setDefunCases: Seq[CaseDefinition] = Seq()
  var setDefunGroupRelations: Map[Type, (String, Seq[Relation])] = Map()

  private def innerSetType(typ: Type) = typ match {
    case TSet(ty) => ty
    case ty => throw IllegalStateException(s"Expected Set, but got $ty")
  }

  private def freshSetRelation(dependencies: Seq[Var], args: Seq[Term], outTyp: Type): Relation = gensym.scoped {
    // TODO: Prefix or let demand transformation handle it aka. depend on demand IR and insert a placeholder
    val dependentParams = dependencies.map { v =>
      Param(v.name, visitType(v.typ.getOrElse(throw IllegalStateException(s"Untyped var $v"))))
    }
    val setName = gensym.fresh("set")
    val outName = gensym.fresh("return")
    val outVar = Var(outName)
    val outParam = Param(outName, innerSetType(outTyp))
    val setParam = Param(setName, visitType(outTyp))
    val rel = Relation(
      gensym.freshGlobal(IR.name.toLowerCase() + "Rel"),
      setParam +: dependentParams :+ outParam,
      Seq(Body(Seq(Disjunction(args.map { t =>
        visitTerm(t).map(tt => Eq(outVar, tt))
      }))))
    )
    rel
  }

  private def visitSetCompare(lhs: Term, rhs: Term): Unit = (lhs.typ, rhs.typ) match
    // TODO: We need to check if a variable is bound
    //case (None, _) => throw IllegalStateException(s"Untyped expression $lhs")
    //case (_, None) => throw IllegalStateException(s"Untyped expression $rhs")
    //case (Some(TSet(_)), Some(TSet(_))) => throw IllegalStateException("Can not compare sets for equality!")
    // TODO: We could let the body fail here
    //case (Some(TSet(_)), _) | (_, Some(TSet(_))) => throw IllegalStateException("Can not compare sets for equality!")
    case _ => // nothing

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = {
    val defunTyName = gensym.fresh("DefunSet")
    setDefunType = Some(TData(defunTyName))
    super.visitModuleEntry(moduleEntry) :+ DataDefinition(defunTyName, setDefunCases)
  }

  override def visitRelation(relation: Relation): Seq[Relation] =
    val rels = super.visitRelation(relation)

    val groupDefunRelations = setDefunGroupRelations.map { case (sig, (groupRelationName, relations)) =>
      val setParamName = "set"
      val outParamName = "out"
      val bodies = relations.map { r =>
        val freeParam = (0 until (r.params.size - 2)).map(i => Var("_$" + i.toString))
        // Note: This relies on the order of arguments inside a set
        val args = Var(setParamName) +: freeParam :+ Var(outParamName)
        Body(Seq(Call(r.name, args)))
      }
      val setParam = Param(setParamName, setDefunType.get)
      val outParams = Param(outParamName, sig)
      Relation(groupRelationName, Seq(setParam, outParams), bodies)
    }
    rels ++ setDefunRelations ++ groupDefunRelations

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Eq(lhs, rhs) =>
      visitSetCompare(lhs, rhs)
      super.visitAtom(atom)
    case Neq(lhs, rhs) =>
      visitSetCompare(lhs, rhs)
      super.visitAtom(atom)
    case SetMember(t1, t2) =>
      // TODO: Fix this
      visitTerm(t1).zip(visitTerm(t2)).map(Eq.apply)
    case _ => super.visitAtom(atom)

  private var defunctionalize = true

  private def refunctionalize[A](f: => A): A = try {
      defunctionalize = false
      val a = f
      a
    } finally {
      defunctionalize = true
    }

  private def refunctionalizeTerm(term: Term): Seq[Term] = refunctionalize {
    term match
      case Set(Seq()) => Seq(Var("EMPTY")) // TODO: Handle empty Set correctly
      case Set(ts) => ts.flatMap(visitTerm)
      case Var(name) if term.typ.exists(_.isInstanceOf[TSet]) =>
        val setTy = innerSetType(term.typ.get)
        val (groupRelName, _) = setDefunGroupRelations(setTy)
        val outVar = gensym.fresh("return")
        Seq(block.Block(
          Seq(Call(groupRelName, Seq(Var(name), Var(outVar)))),
          Var(outVar)
        ))
      case SetUnion(t1, t2) => refunctionalizeTerm(t1) ++ refunctionalizeTerm(t2)
      case SetIntersection(t1, t2) =>
        val lhsTerms = refunctionalizeTerm(t1)
        val rhsTerms = refunctionalizeTerm(t2)
        val baseCase = refunctionalizeTerm(Set.empty)
        lhsTerms.flatMap { lhs =>
          rhsTerms.map { rhs =>
            block.Block(Seq(Eq(lhs, rhs)), lhs)
          }
        } ++ baseCase
      case _ => visitTerm(term)
  }

  private def defunctionalizeTerm(term: Term): Seq[Term] = {
    val dependentVars = term.vars.distinct
    val ty = term.typ.getOrElse(throw IllegalStateException(s"Untyped expression $term"))
    val relation = freshSetRelation(dependentVars, refunctionalizeTerm(term), ty)
    setDefunRelations :+= relation
    val dependentTys = dependentVars.map(v => v.typ.getOrElse(throw IllegalStateException(s"Untyped var $v")))
    val caseName = gensym.freshGlobal(IR.name)
    setDefunCases :+= CaseDefinition(caseName, dependentTys.map(visitType))

    // we can not identify the specific relation that belongs to a variable,
    // but we can group set relations with the same type

    // TODO: this is not enough. We would need to group by subtype relation.
    //  That is:
    //  for ((k, _) <- setDefunGroupRelations)
    //    if (subType(setTy, k))
    //      setDefunGroupRelations += setTy -> (setDefunGroupRelations(setTy)._1, setDefunGroupRelations(setTy)._2 :+ relation)
    //      setDefunGroupRelations += k -> (setDefunGroupRelations(k)._1, setDefunGroupRelations(k)._2 :+ relation)
    //  Otherwise code like this will not work:
    //    x = Set(Int(1), Int(2))
    //    SomeRel(x: Set[Num]) :- ...
    //    SomeRel(x)
    //  The type of x is now Set[Num], that means setGroup$Num is used, but
    //  x belongs to setGroup$Int.
    //  Solution:
    //   Each module should additionally include the "class" hierarchy.
    //   Subtype(Type, Type) extends ModuleEntry ??
    val setTy = innerSetType(ty)
    val (groupRelName, groupRelations) = setDefunGroupRelations.getOrElse(setTy, (gensym.fresh("setGroupRel"), Seq()))
    setDefunGroupRelations += setTy -> (groupRelName, groupRelations :+ relation)

    val defunVar = Var(gensym.fresh("setObj"))
    Seq(block.Block(
      Seq(
        Eq(defunVar, Construct(caseName, dependentVars)),
        // TODO: If we manually copy the prefix then we don't need this call
        //  Otherwise this call should "write" aka. generate demand
        Call(relation.name, defunVar +: dependentVars.flatMap(visitTerm) :+ Var(gensym.fresh("return")))
      ),
      defunVar
    ))
  }

  override def visitTerm(term: Term): Seq[Term] = term match
    case Set(Seq()) => ??? // TODO: Handle empty Set
    case Set(_) | SetUnion(_, _) | SetIntersection(_, _) =>
      if (defunctionalize)
        defunctionalizeTerm(term)
      else
        refunctionalizeTerm(term)
    // TODO: Handle Set Intersection + include base case for empty set
    //case SetIntersection(t1, t2) => ???
    // Group relations with the same signature for aggregations like so:
    // aggregateSet(x, ....) :- set$0(x, ....) or set$1(x, ....)
    case _ => super.visitTerm(term)

  override def visitType(ty: Type): Type = ty match
    case TSet(ty) => setDefunType.get
    case _ => super.visitType(ty)