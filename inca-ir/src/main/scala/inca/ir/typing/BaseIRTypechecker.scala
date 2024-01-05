package inca.ir.typing

import inca.ir.util.SourceLocation
import inca.ir.*
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq

// We assume that every variable that is used is introduced beforehand (left-to-right)
trait BaseIRTypechecker extends BaseIRTypeContext:

  // Always process Relations last
  implicit def ordering[A <: ModuleEntry]: Ordering[A] = (x: A, y: A) => (x, y) match
    case (r1: Relation, r2: Relation) => 0
    case (_, r2: Relation) => -1
    case (r1: Relation, _) => 1
    case _ => 0

  def checkProgram(program: Seq[Module]): Unit = scopedTypeContext {
    program.foreach(bindModule)
    program.foreach(checkModule)
  }

  def checkModule(module: Module): Unit = scopedTypeContext {
    module.contents.sorted.foreach(bindModuleEntry)
    module.contents.sorted.foreach(checkModuleEntry)
    this.failOnError()
  }

  def bindModuleEntry(entry: ModuleEntry): Unit =
    registerModuleEntry(entry)

  def checkModuleEntry(moduleEntry: ModuleEntry): Unit = moduleEntry match {
      case relation: Relation => scopedTypeContext { checkRelation(relation) }
      case relation: ExtensionalRelation => // nothing
      case _ => throw IllegalArgumentException(s"Can not typecheck unknown entry: $moduleEntry")
  }

  def checkRelation(relation: Relation): Unit = {
    // bind parameters
    relation.params.foreach(checkParam)
    relation.bodies.foreach(b => scopedTypeContext {
      checkBody(b)
      relation.params.foreach { p =>
        if (!isBoundVar(p.name))
          error(s"Parameter $p is not positively bound in relation \"${relation.name}\", body \n$b")
      }
    })
  }

  def checkType(ty: Type): Unit = ty match
    case TAny | TNothing => // good
    case _ => throw IllegalArgumentException(s"Cannot check unknown type: $ty")

  def checkParam(param: Param): Unit =
    checkType(param.ty)
    registerVar(param.name, param, param.ty)

  def checkBody(body: Body): Unit =
    body.atoms.foreach(at => checkAtom(at, Mode.Binding))

  def assertComparable(ty: Type, outside: Type, t: SourceLocation): Unit =
    if (ty != outside)
      error(s"$t of type $ty is not comparable to $outside", t)

  def checkTerm(term: Term, expected: Type, mode: Mode): Mode =
    assignType(term) {
      val cl = checkTermExtend(term, expected, mode)
      TermType(expected, cl)
    }._2

  final def inferTerm(term: Term, mode: Mode): TermType =
    assignType(term) {
      inferTermExtend(term, mode)
    }

  protected def checkTermExtend(term: Term, expected: Type, mode: Mode): Mode = term match
    case v@Var(RefByName(name)) => mode match
      case Mode.Binding => lookupVar(name) match
        case None =>
          registerVar(name, v, expected)
          bindVar(name)
          Mode.Binding
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          assertComparable(ty, expected, v)
          bindVar(name)
          Mode.Binding
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          assertComparable(ty, expected, v)
          Mode.Bound
      case Mode.Bound => lookupVar(name) match
        case None =>
          error(s"Undefined variable $v at closed position", v)
          Mode.Bound
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          error(s"Unbound variable $v at closed position", v)
          assertComparable(ty, expected, v)
          Mode.Bound
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          assertComparable(ty, expected, v)
          Mode.Bound
      case Mode.Collapse => lookupVar(name) match
        case None =>
          Mode.Collapse
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          assertComparable(ty, expected, v)
          Mode.Collapse
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          assertComparable(ty, expected, v)
          Mode.Bound

    case _ => // fallback to infer + compatibility check
      val TermType(ty,m) = inferTerm(term, mode)
      assertComparable(ty, expected, term)
      m

  protected def inferTermExtend(term: Term, mode: Mode): TermType = term match
    case v@Var(RefByName(name)) => mode match
      case Mode.Binding => lookupVar(name) match
        case None =>
          error(s"Cannot infer type of Undefined variable $v", v)
          registerVar(name, v, TAny)
          bindVar(name)
          TAny.binding
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          bindVar(name)
          ty.binding
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          ty.bound
      case Mode.Bound => lookupVar(name) match
        case None =>
          error(s"Undefined variable $v at closed position", v)
          TAny.bound
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          error(s"Unbound variable $v at closed position", v)
          ty.bound
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          ty.bound
      case Mode.Collapse => lookupVar(name) match
        case None =>
          TAny.collapsed
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          ty.collapsed
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          ty.bound

    case Cast(t, ty) =>
      checkType(ty)
      val m = withErrors(inferTerm(t, mode)) match
        case (tt, Nil) => tt.mode
        case (tt,errsInfer) =>
          withErrors(checkTerm(t, ty, mode)) match
            case (m, Nil) => m
            case (_,errsCheck) =>
              errsInfer.foreach(e => error(e.msg, e.sourceLocations:_*))
              tt.mode
      TermType(ty, m)
    case _ => throw IllegalArgumentException(s"Can not typecheck unknown term: $term")

  def checkCall(ref: Ref[Relation], args: Seq[Arg], atom: Atom, mode: Mode): Unit =
    val paramTys = inferRelationRef(ref, atom)
    if (paramTys.size != args.size)
      error(s"Expected ${paramTys.size} arguments but got: ${args.size}", atom)
    val argMode = mode match
      case Mode.Binding => Mode.Binding
      case Mode.Bound => Mode.Collapse
      case Mode.Collapse => Mode.Collapse
    args.zipAll(paramTys, null, null).foreach {
      case (wildcard@WildcardArg(), ty) =>
        wildcard.typed(ty.collapsed, force = true)
      case (TermArg(t), null) => // missing param
        inferTerm(t, argMode)
      case (null, _) => // missing argument
        // nothing
      case (TermArg(t), ty) =>
        checkTerm(t, ty, argMode)
    }

  def inferRelationRef(ref: Ref[Relation], s: SourceLocation): Seq[Type] = ref match
    case RefByName(name) => lookupModuleEntry(name) match
      case Some(Relation(_, params, _)) => params.map(_.ty)
      case Some(ExtensionalRelation(_, params)) => params.map(_.ty)
      case None =>
        error(s"Undefined relation $name", s)
        Seq()
      case entry =>
        error(s"Expected a relation $name but found $entry", s)
        Seq()

  def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Call(ref, args, false) => checkCall(ref, args, atom, mode)
    case Call(ref, args, true) => checkCall(ref, args, atom, mode.inverted)
    case ExtensionalCall(name, args, false) => checkCall(name, args, atom, mode)
    case ExtensionalCall(name, args, true) => checkCall(name, args, atom, mode.inverted)

    case Eq(lhs, rhs, false) =>
      val action = startContextTransaction()
      withErrors(inferTerm(lhs, Mode.Bound)) match
        case (TermType(lty,_), Nil) =>
          action.commit()
          checkTerm(rhs, lty, mode)
        case (_, lerrs) =>
          action.abort()
          withErrors(inferTerm(rhs, Mode.Bound)) match
            case (TermType(rty,_), Nil) => checkTerm(lhs, rty, mode)
            case (_, rerrs) =>
              error(s"Ill-typed equation, cannot infer closed type for either side", atom)
              lerrs.foreach(e => error(e.msg, e.sourceLocations:_*))
              rerrs.foreach(e => error(e.msg, e.sourceLocations:_*))

    case Eq(lhs, rhs, true) =>
      val action = startContextTransaction()
      withErrors(inferTerm(lhs, Mode.Bound)) match
        case (TermType(lty,_), Nil) =>
          action.commit()
          checkTerm(rhs, lty, mode.inverted)
        case (_, lerrs) =>
          action.abort()
          withErrors(inferTerm(rhs, Mode.Bound)) match
            case (TermType(rty,_), Nil) => checkTerm(lhs, rty, mode.inverted)
            case (_, rerrs) =>
              error(s"Ill-typed equation, cannot infer closed type for either side", atom)
              lerrs.foreach(e => error(e.msg, e.sourceLocations: _*))
              rerrs.foreach(e => error(e.msg, e.sourceLocations: _*))

    case _ =>
      throw IllegalStateException(s"Can not typecheck unknown atom: $atom")


  private def assignType(term: Typeable[TermType] with SourceLocation)(computeType: => TermType): TermType =
    val inferred = computeType
    term.typed(inferred, force = true)
    inferred

  protected def checkAlternatives[A <: SourceLocation](as: Iterable[A])(f: A => Unit): Unit =
    if (as.isEmpty) {
      // do nothing
    } else {
      val a = as.head
      val rest = as.tail
      val varsBefore = this.vars
      f(a)
      var varsAfter = this.vars
      rest.foreach { a =>
        this.vars = varsBefore
        f(a)
        val varsAfterThis = vars
        // remove variables not bound by this alternative
        varsAfter = varsAfter.filter(kv => varsAfterThis.contains(kv._1))
        for ((x, VarInfo(_, ty2, vmode2)) <- varsAfterThis) varsAfter.get(x) match
          case Some(VarInfo(trg1, ty1, vmode1)) if ty1 == ty2 =>
            varsAfter += x -> VarInfo(trg1, ty1, vmode1 && vmode2)
          case Some(VarInfo(trg1, ty1, vmode1)) =>
            error(s"Alternative has conflicting type for variable $x: $ty2 instead of $ty1", a)
            varsAfter += x -> VarInfo(trg1, TAny, vmode1 && vmode2)
          case None => // nothing
      }
      this.vars = varsAfter
    }
