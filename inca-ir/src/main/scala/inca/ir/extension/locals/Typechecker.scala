package inca.ir.extension.locals

import inca.ir.*
import inca.ir.typing.{BaseIRTypechecker, Mode}
import inca.ir.extension.locals.*

trait Typechecker extends BaseIRTypechecker:

  var usesExplicitLocals: Boolean = false

  def withExplicitLocals[A](explicitLocals: Boolean)(f: => A): A =
    val oldWithLocals = usesExplicitLocals
    usesExplicitLocals = explicitLocals
    val res = f
    usesExplicitLocals = oldWithLocals
    res

  override protected def checkModule(module: Module): Unit =
    // If a module uses this extension we change the behaviour of Eq constraints and variable shadowing.
    val useExplicitLocals = module.lang.features.contains(inca.ir.extension.locals.IR)
    withExplicitLocals(useExplicitLocals) {
      super.checkModule(module)
    }

  private var mutableVars: Set[Name] = Set()

  override def scopedTypeContext[T](f: => T): T =
    val mutableVarsSaved = mutableVars
    try super.scopedTypeContext(f)
    finally mutableVars = mutableVarsSaved

  override def registerVar(name: Name, decl: Var.Target, ty: Type): Unit =
    if (usesExplicitLocals) {
      val (_, errs) = withErrors(super.registerVar(name, decl, ty))
      val isShadowed = errs.nonEmpty
      if (isShadowed && !mutableVars.contains(name)) {
        errs.foreach(e => error(e.msg, e.sourceLocations *))
      }
    } else {
      super.registerVar(name, decl, ty)
    }

  override def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case DeclVal(v@Var(ref), t) =>
      val ty = inferTerm(t, Mode.Bound).ty
      registerVar(ref.name, v, ty)
      bindVar(ref.name)

    case DeclVar(v@Var(ref), t) =>
      val ty = inferTerm(t, Mode.Bound).ty
      registerVar(ref.name, v, ty)
      mutableVars += ref.name
      bindVar(ref.name)

    case Assign(v@Var(ref), t) =>
      val ty = inferTerm(t, Mode.Bound).ty
      lookupVar(ref) match
        case Some(varInfo) if mutableVars.contains(ref.name) =>
          assertComparable(varInfo.ty, ty, atom)
        case Some(_) =>
          error(s"Can not assign immutable variable $v", v, atom)
        case None =>
          error(s"Can not assign unbound variable $v", v, atom)

    // If we use explicit locals we disallow assigning variables with Eq constraints.
    // That means, non-negative equality constraints are only comparisons, so both sides must be bound.
    case Eq(lhs, rhs, false) if usesExplicitLocals =>
      val (_, errs) = withErrors {
        val lhsTy = inferTerm(lhs, Mode.Bound).ty
        val rhsTy = inferTerm(rhs, Mode.Bound).ty
        assertComparable(lhsTy, rhsTy, atom)
      }
      errs.foreach(e => error(e.msg, e.sourceLocations *))

    case _ => super.checkAtom(atom, mode)

  override def checkBody(body: Body): Unit = super.checkBody(body)