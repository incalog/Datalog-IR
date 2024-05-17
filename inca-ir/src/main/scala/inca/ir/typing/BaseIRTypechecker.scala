package inca.ir.typing

import inca.ir.util.SourceLocation
import inca.ir.*
import inca.ir.visitors.BaseIRVisitor

import scala.collection.immutable.Seq
import scala.reflect.ClassTag

// We assume that every variable that is used is introduced beforehand (left-to-right)
trait BaseIRTypechecker extends BaseIRTypeContext:

  // Always process Relations last
  implicit def ordering[A <: ModuleEntry]: Ordering[A] = (x: A, y: A) => (x, y) match
    case (r1: Relation, r2: Relation) => 0
    case (_, r2: Relation) => -1
    case (r1: Relation, _) => 1
    case _ => 0

  def checkProgram(program: Seq[Module], dependencies: Seq[Module] = Seq()): Unit = scopedTypeContext {
    assert(dependencyGraph.nodes.isEmpty, "Type checking needs to be started with a fresh type checker instance.")

    //program.foreach(println)
    //println("---------------")
    //dependencies.foreach(println)

    dependencies.foreach(bindModule)

    dependencies.foreach { module =>
      module.contents.sorted.foreach(e => bindHeaderEntry(e)(module))
      module.imports.foreach(i => bindModuleImport(i)(module))
    }

    //println(subst.toSeq.map { case ((m, n), _) => m.name -> n })

    program.foreach(checkModule)

    val negativeCycles = dependencyGraph.negativeCycles
    negativeCycles.foreach( cycle =>
      val cycleS = dependencyGraph.prettyPrintCycle(cycle.map(_._1))
      error(s"Negative cycle is not allowed:\n  $cycleS", program:_*)
    )

    this.failOnError()
  }

  protected var currentModule: Module = _
  protected var currentEntry: ModuleEntry = _

  def addCallDependency(to: ModuleEntry, neg: Boolean = false): Unit =
    addDependency(currentEntry, to, if (neg) DependencyInfo.NegativeCall else DependencyInfo.PositiveCall)
  def addTypeDependency(to: ModuleEntry): Unit =
    addDependency(currentEntry, to, DependencyInfo.TypeReference)

  protected def checkModule(module: Module): Unit = scopedTypeContext {
    currentModule = module
    module.contents.sorted.foreach(e => bindModuleEntry(e))
    module.imports.foreach(i => bindModuleImport(i)(module))

    module.contents.sorted.foreach { entry =>
      currentEntry = entry
      checkModuleEntry(entry)
    }
  }

  protected def bindHeaderEntry(entry: ModuleEntry)(implicit module: Module): Unit = entry match
    case p: Provide[_] => registerProvide(p)(module)
    case r: Require => registerRequire(r)(module)
    case _ => // nothing

  protected def bindModuleEntry(entry: ModuleEntry): Unit = entry match
    case _: Provide[_] => // do not register provides. We either have a "require" or another module entry with this name
    case _ => registerModuleEntry(entry)

  protected def checkRequire[T <: ModuleEntry](require: Require): Unit = require match
    case _: RequireRelation => // nothing, we check these on import
    case _ => error(s"Can not typecheck unknown require: $require")

  protected def checkProvide[T <: Providable](provide: Provide[T]): Unit = provide match
    case p: ProvideRelation =>
      val sig = inferRelationRef(p.exportRef, provide)
      if sig.size != p.params.size then
        error(s"Expected ${sig.size} parameters, but got ${p.params.size}", provide)
      p.params.zip(sig).foreach {
        case (param, ty) => assertComparable(param.ty, ty, provide)
      }
    case _ => error(s"Can not typecheck unknown provide: $provide")

  protected def checkImport(imp: Import): Unit = imp match
    case Import(moduleRef, as, entries) =>
      moduleRef.target match
        case Some(mod) =>
          // Make sure every substitution is valid
          entries.foreach(i => checkSubstitution(imp, i))
          // Make sure every required entry in the imported module is satisfied
          val required = mod.contents.collect { case req: Require => req }.sortBy(_.name.name)
          val importedRequired = entries.map(_.to).sortBy(_.name.name)
          if required.size != importedRequired.size then
            error(s"Expected ${required.size} requirements, but got ${importedRequired.size}", imp)
          required.zip(importedRequired).foreach { (req, imp) =>
            (imp.target, req) match
              case (Some(reqTarget), req) if reqTarget != req =>
                error(s"Incorrectly resolved requirement ${req.name}", imp)
              case _ => // ok
          }
        case _ => error(s"Unresolved module $moduleRef", imp)


  protected def checkSubstitution(imp: Import, importable: Substitution[_]): Unit = importable match
    case RelationSubstitution(to, toSig, from, fromSig) =>
      // Make sure the to and from signature match
      if toSig.size != fromSig.size then
        error(s"Expected ${toSig.size} parameters, but got ${fromSig.size}", importable)
      fromSig.zip(toSig).foreach {
        case (fromParam, toParam) => assertComparable(fromParam.ty, toParam.ty, importable)
      }
      // Make sure there is a "require" for the "to" name and resolve it
      lookupRequireRef(to, importable, imp.module.target.get)
      inferRelationRef(from, importable)
    case _ => error(s"Can not typecheck import: $importable")

  protected def lookupRequireRef[R <: Require](ref: Ref[R], s: SourceLocation, module: Module)(implicit tag: ClassTag[R]): Option[R] =
    lookupRequire[R](ref.name, module)(tag) match
      case Some(r) =>
        ref.resolved(r)
        Some(r)
      case _ =>
        error(s"Could not resolve entry required by: ${ref.name}", s)
        None

  protected def checkModuleEntry(moduleEntry: ModuleEntry): Unit = scopedTypeContext {
    moduleEntry match
      case relation: Relation => checkRelation(relation)
      case relation: ExtensionalRelation => // nothing
      case r: Require => checkRequire(r)
      case p: Provide[_] => checkProvide(p)
      case i: Import => checkImport(i)
      case _ => throw IllegalArgumentException(s"Can not typecheck unknown entry: $moduleEntry")
  }

  protected def checkRelation(relation: Relation): Unit = {
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

  protected def checkType(ty: Type): Unit = ty match
    case TAny | TNothing => // good
    case _ => throw IllegalArgumentException(s"Cannot check unknown type: $ty")

  protected def checkParam(param: Param): Unit =
    checkType(param.ty)
    registerVar(param.name, param, param.ty)

  protected def checkBody(body: Body): Unit =
    body.atoms.foreach(at => checkAtom(at, Mode.Binding))

  protected def assertComparable(ty: Type, outside: Type, t: SourceLocation): Unit =
    if (ty != outside)
      error(s"$t of type $ty is not comparable to $outside", t)

  protected def checkTerm(term: Term, expected: Type, mode: Mode): Mode =
    assignType(term) {
      val cl = checkTermExtend(term, expected, mode)
      TermType(expected, cl)
    }._2

  final protected def inferTerm(term: Term, mode: Mode): TermType =
    assignType(term) {
      inferTermExtend(term, mode)
    }

  protected def checkTermExtend(term: Term, expected: Type, mode: Mode): Mode = term match
    case v@Var(ref) => mode match
      case Mode.Binding => lookupVar(ref) match
        case None =>
          registerVar(ref.name, v, expected)
          bindVar(ref.name)
          Mode.Binding
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          assertComparable(ty, expected, v)
          bindVar(ref.name)
          Mode.Binding
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          assertComparable(ty, expected, v)
          Mode.Bound
      case Mode.Bound => lookupVar(ref) match
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
      case Mode.Collapse => lookupVar(ref) match
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
    case v@Var(ref) => mode match
      case Mode.Binding => lookupVar(ref) match
        case None =>
          error(s"Cannot infer type of Undefined variable $v", v)
          registerVar(ref.name, v, TAny)
          bindVar(ref.name)
          TAny.binding
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          bindVar(ref.name)
          ty.binding
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          ty.bound
      case Mode.Bound => lookupVar(ref) match
        case None =>
          error(s"Undefined variable $v at closed position", v)
          TAny.bound
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          error(s"Unbound variable $v at closed position", v)
          ty.bound
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          ty.bound
      case Mode.Collapse => lookupVar(ref) match
        case None =>
          TAny.collapsed
        case Some(VarInfo(_, ty, VarMode.Unbound)) =>
          ty.collapsed
        case Some(VarInfo(_, ty, VarMode.Bound)) =>
          ty.bound

    case Cast(t, ty) =>
      checkType(ty)
      val action = startContextTransaction()
      val m = withErrors(inferTerm(t, mode)) match
        case (tt, Nil) =>
          action.commit()
          tt.mode
        case (tt,errsInfer) =>
          action.abort()
          withErrors(checkTerm(t, ty, mode)) match
            case (m, Nil) => m
            case (_,errsCheck) =>
              errsInfer.foreach(e => error(e.msg, e.sourceLocations:_*))
              tt.mode
      TermType(ty, m)
    case _ => throw IllegalArgumentException(s"Can not typecheck unknown term: $term")

  protected def checkCall[R <: ModuleEntry](ref: Ref[R], args: Seq[Arg], atom: Atom, mode: Mode)(implicit tag: ClassTag[R]): Unit =
    val paramTys = inferRelationRef(ref, atom)
    ref.target.foreach(addCallDependency(_, !mode.isBinding))
    if (paramTys.size != args.size)
      error(s"Expected ${paramTys.size} arguments but got: ${args.size}", atom)
    val argMode = mode match
      case Mode.Binding => Mode.Binding
      case Mode.Bound => Mode.Collapse
      case Mode.Collapse => Mode.Collapse
    args.zipAll(paramTys, null, null).foreach {
      case (wildcard@WildcardArg(), null) =>
        // if inferRelationRef fails, we do not want to exit with a null pointer
        error("Could not infer type for wildcard.", atom)
        wildcard.typed(TAny.collapsed, force = true)
      case (wildcard@WildcardArg(), ty) =>
        wildcard.typed(ty.collapsed, force = true)
      case (TermArg(t), null) => // missing param
        inferTerm(t, argMode)
      case (null, _) => // missing argument
        // nothing
      case (TermArg(t), ty) =>
        checkTerm(t, ty, argMode)
    }

  protected def lookupModulePath[R <: ModuleEntry](ref: Ref[R], s: SourceLocation*)(implicit tag: ClassTag[R]): Module =
    ref match
      case RefByQualifiedName(ns) =>
        // resolve the modules in the path
        val m: Option[Module] = None
        ns.dropRight(1).foldLeft(m) {
          case (_, moduleName) => lookupModuleByAlias(moduleName)(currentModule) match
            case Some(mod) => 
              Some(mod)
            case _ =>
              error(s"Could not resolve module $moduleName", s:_*)
              None
        }.getOrElse(currentModule)
      case _ => currentModule
  
  protected def inferRelationRef[R <: ModuleEntry](ref: Ref[R], locations: SourceLocation*)(implicit tag: ClassTag[R]): Seq[Type] =
    val targetModule = lookupModulePath(ref, locations:_*)
    val (rel, tys) = if targetModule != currentModule then
      // definitions outside the current module must be provided
      val providedRel = lookupProvideRef[ProvideRelation](ref, targetModule, locations:_*)
      (providedRel, providedRel.map(_.params.map(_.ty)).getOrElse(Seq()))
    else
      // definitions inside the module can either be a relation or a requirement
      lookupRelationRef[R](ref, locations:_*)
    rel.map(r => ref.resolved(r.asInstanceOf[R]))
    tys
  
  protected def lookupProvideRef[P <: Provide[_]](ref: Ref[_], module: Module, locations: SourceLocation*)(implicit tag: ClassTag[P]): Option[P] =
    lookupProvide[P](ref.unqualifiedName, module) match
      case prov@Some(ProvideRelation(_, params)) => prov
      case _ =>
        error(s"Could not resolve entry provided by: ${ref.name}", locations:_*)
        None

  // lookup a relation in a module given a name
  private def lookupRelationRef[R <: ModuleEntry](ref: Ref[R], s: SourceLocation*)(implicit tag: ClassTag[R]): (Option[R], Seq[Type]) =
    lookupModuleEntry(ref.unqualifiedName) match
      case Some(rel@Relation(_, params, _)) =>
        if (!tag.runtimeClass.isInstance(rel))
          error(s"Expected ${tag.runtimeClass.getSimpleName}, but got ${rel.getClass.getSimpleName} while resolving RelationRef", s:_*)
        (Some(rel.asInstanceOf[R]), params.map(_.ty))
      case Some(rel@ExtensionalRelation(_, params)) =>
        // This might e.g. happen if we perform a normal call on an extensional relation
        if (!tag.runtimeClass.isInstance(rel))
          error(s"Expected ${tag.runtimeClass.getSimpleName}, but got ${rel.getClass.getSimpleName} while resolving RelationRef", s:_*)
        (Some(rel.asInstanceOf[R]), params.map(_.ty))
      case Some(req@RequireRelation(_, params)) =>
        (Some(req.asInstanceOf[R]), params.map(_.ty))
      case None =>
        error(s"Undefined relation ${ref.name}", s:_*)
        (None, Seq())
      case entry =>
        error(s"Expected a relation ${ref.name} but found $entry", s:_*)
        (None, Seq())

  protected def checkAtom(atom: Atom, mode: Mode): Unit = atom match
    case Call(ref, args, false) => checkCall(ref, args, atom, mode)
    case Call(ref, args, true) => checkCall(ref, args, atom, mode.inverted)
    case ExtensionalCall(ref, args, false) => checkCall(ref, args, atom, mode)
    case ExtensionalCall(ref, args, true) => checkCall(ref, args, atom, mode.inverted)

    case Eq(lhs@Var(x), rhs, false) if !lookupVar(x).exists(_.mode == VarMode.Bound) =>
      // special case to avoid backtracking for ubiquitous `x = e`  
      val rty = inferTerm(rhs, Mode.Bound).ty
      checkTerm(lhs, rty, mode)
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
