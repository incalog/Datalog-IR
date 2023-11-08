package inca.frontend.oodl.typechecker

// TODO: Support generics
// TODO: Support Set fold
// TODO: assign variables to method calls that return unit is not allowed
// TODO: Check that each path returns

import inca.frontend.oodl.syntax.*
import inca.frontend.oodl.util.ParseUtil
import inca.ir.Name
import inca.ir.typing.{Resolvable, Typeable}
import inca.ir.util.SourceLocation

class Typechecker extends TypeContext with TypeIO:
  val builtinModule: Module = Module(
    Name("builtin"), Seq(), Seq(
      ClassDef(Seq(), None, Name("Object"), Seq(), Seq(), Seq(
        ConstructorDef(Seq(), None, Seq(), Seq())
      ))
    )
  )

  def typecheck(program: Seq[Module]): Unit = scopedTypeContext {
    program.foreach(bindModule)
    program.foreach(typecheck)
  }

  def typecheck(module: Module): Unit = scopedTypeContext {
    // Bind all builtins
    builtinModule.classes.foreach(bindClass(_, builtinModule))

    val moduleNames = module.name +: module.imports.map(_.name)
    val classNames = module.classes.map(_.name)

    for (name <- moduleNames.toSet.intersect(classNames.toSet))
      error(s"Module $name is shadowed by class $name", name)

    for (imp <- module.imports; importedModule <- lookupModule(imp.name)) {
      resolveTarget(imp)(importedModule)

      for (clazz <- importedModule.classes if !clazz.vis.contains(Private))
        bindClass(clazz, importedModule)
    }

    // detect inheritance cycles
    /*val inheritanceGraph = InheritanceGraph(module.classes)
    inheritanceGraph.cycles.foreach { c =>
      error(s"Cycle in inheritance hierarchy: ${c.map(_.name.raw).mkString(" <- ")}", c: _*)
    }

    if (inheritanceGraph.cycles.nonEmpty)
      return
    */

    // bind symbols first
    module.classes.foreach(bindClass(_, module))
    // resolve all parent class refs before typechecking any and thereby check the inheritance
    module.classes.foreach { cls =>
      cls.parentCls.foreach { pCls =>
        resolveNamedType(pCls) match
          case Some(_ : ClassDef) => // nothing
          case _ =>
            error(s"Class ${cls.name} can not inherit from none class type $pCls", cls)
      }
    }

    module.classes.foreach(typecheck)

    // TODO: typecheck main function
    module.functions.foreach(typecheck)
  }

  /** ModuleContent */

  def typecheck(functionDef: FunctionDef): Unit = {
    val hasMainAnno = functionDef.annos.exists(_.isInstanceOf[MainFunctionAnno])
    if (!hasMainAnno)
      error(s"Function ${functionDef.name} is not a main function")

    functionDef.params.foreach { p =>
      typecheck(p.typ)
      bindVar(p.name, p, p.typ, immutable = true)
    }
    typecheck(functionDef.outType)
    typecheck(functionDef.body, functionDef.outType)(None)
  }

  var uninitializedFields: Map[Name, FieldDef] = Map()

  def typecheck(classDef: ClassDef): Unit = {
    // Note: We only allow a single constructor
    classDef.contentMap.foreach {
      case (_, cs) if cs.size > 1 =>
        error(s"Ambiguous names in class '${classDef.name}'", cs: _*)
      case _ => // nothing
    }

    // Case classes are immutable
    if (classDef.isCaseClass) {
      classDef.fields.filter(!_.immutable).foreach { f =>
        error(s"Case Class ${classDef.name} must not contain mutable field ${f.name}", f)
      }
      classDef.fields.filter(!_.isGeneratedConstructorField).foreach { f =>
        error(s"Case Class ${classDef.name} must not contain explicit field ${f.name} outside the constructor", f)
      }

      val allFields = collect[FieldDef](classDef)(_.isInstanceOf[FieldDef])
      val parentFields = allFields.filter((c, _) => c != classDef)
      if (parentFields.nonEmpty)
        error(s"Case class ${classDef.name} must no inherit fields", classDef)
    }

    // make sure all fields are initialized after a constructor is executed
    uninitializedFields = Map()

    classDef.fields.foreach(f => typecheck(f, classDef))
    classDef.methods.foreach(m => typecheck(m, classDef))
    classDef.constructors.foreach { constructor =>
      // every path trough a constructor must initialize all fields
      val storeUninitializedFields = uninitializedFields
      typecheck(constructor, classDef)
      uninitializedFields.foreach { case (fieldName, fieldDef) =>
        error(s"Field '$fieldName' is not initialized", fieldDef)
      }
      uninitializedFields = storeUninitializedFields
    }
  }

  /** Typing */

  def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match {
    case (_, TAny) => true
    case (TNull, TNull) => true
    case (TNull, t: TName) if t.target.exists(_.isInstanceOf[ClassDef]) => true
    case (TNull, _) => false
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      tys1.zip(tys2).forall(tt => subtype(tt._1, tt._2))
    case (TSet(ty1), TSet(ty2)) =>
      subtype(ty1, ty2)
    case (t1: TName, t2: TName) if t1.isBuiltIn && t2.isBuiltIn => t1.name == t2.name
    case (t1: TName, t2: TName) if t1.isBuiltIn => false
    case (t1: TName, t2: TName) if t2.isBuiltIn => false
    case (t1: TName, t2: TName) =>
      (t1.target, t2.target) match
        case (Some(c1: ClassDef), Some(c2: ClassDef)) if c1 == c2 =>
          true
        case (Some(c1: ClassDef), Some(c2: ClassDef)) =>
          c1.parentCls.exists(t => subtype(t, t2))
        case _ =>
          throw IllegalAccessException(s"Can not compare subtype of types $ty1 and $ty2 with unresolved targets")
    case _ => false
  }

  protected def meet(tys: Iterable[Type]): Type =
    tys.foldLeft[Type](TAny)(meet)

  def meet(ty1: Type, ty2: Type): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      TTuple(tys1.zip(tys2).map(tt => meet(tt._1, tt._2)))
    case (TSet(ty1), TSet(ty2)) => meet(ty1, ty2)
    case (_, TNull) => TNull
    case (TNull, _) => TNull
    case (t1: TName, t2: TName) if !t1.isBuiltIn && !t2.isBuiltIn =>
      if (subtype(ty1, ty2)) ty1
      else ty2
    case _ =>
      if (subtype(ty1, ty2)) ty2
      else if (subtype(ty2, ty1)) ty1
      else TAny
  }

  def join(ty1: Type, ty2: Type): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      TTuple(tys1.zip(tys2).map(tt => join(tt._1, tt._2)))
    case (TSet(ty1), TSet(ty2)) => join(ty1, ty2)
    case (_, TNull) => ty1
    case (TNull, _) => ty2
    case (t1: TName, t2: TName) if !t1.isBuiltIn && !t2.isBuiltIn =>
      if (subtype(ty1, ty2)) ty2
      else if (subtype(ty2, ty1)) ty1
      else
        // find a common supertype
        (t1.target, t2.target) match
          case (Some(c1: ClassDef), Some(c2: ClassDef)) =>
           c1.parentCls.find(pTy => subtype(ty2, pTy)).getOrElse(TAny)
          case _ =>
            throw IllegalAccessException(s"Can join types $ty1 and $ty2 with possible unresolved targets")
    case _ =>
      if (subtype(ty1, ty2)) ty2
      else if (subtype(ty2, ty1)) ty1
      else TAny
  }

  def join(types: Seq[Type]): Type = types.reduce[Type] {
    case (ty1, ty2) => join(ty1, ty2)
  }

  def assertSubtype(ty1: Type, ty2: Type, loc: SourceLocation): Unit = {
    if (!subtype(ty1, ty2))
      error(s"Expected '$ty2', but got '$ty1'", loc)
  }

  def assignType(term: Typeable[Type] with SourceLocation)(computeType: => Type): Type =
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated))
          error(s"Inferred type '$inferred', but expected annotated type '$annotated'", term)
        annotated
      case None =>
        term.typed(inferred)
        inferred
    }

  /** Class content */

  def typecheck(fieldDef: FieldDef, classDef: ClassDef): Unit = {
    if (fieldDef.isGeneratedConstructorField)
      lookupField(classDef, fieldDef.name) match
        case Some((cls, f)) => resolveTarget(fieldDef)(cls)
        case _ => throw IllegalStateException(s"No field found with name '${fieldDef.name}'")
    else
      resolveTarget(fieldDef)(classDef)
    typecheck(fieldDef.typ)

    fieldDef.body match {
      case Some(expr) =>
        val expTyp = typecheck(expr)(Some(classDef))
        assertSubtype(expTyp, fieldDef.typ, fieldDef)
      case None =>
        uninitializedFields += (fieldDef.name -> fieldDef)
    }
  }

  def typecheck(methodDef: MethodDef, classDef: ClassDef): Unit = scopedTypeContext {
    resolveTarget(methodDef)(classDef)

    // get all overridden methods and assign them the same signature
    val overriddenMethods = lookupMethodCandidates(classDef, methodDef.name, methodDef.params.map(_.typ))

    // All methods in the hierarchy except for the highest need an override annotation
    if (overriddenMethods.size > 1) {
      if (!methodDef.annos.exists(_.isInstanceOf[OverrideFunctionAnno]))
        error(s"Method '${methodDef.name}' in class ${classDef.name} is missing an override annotation", methodDef)

      // make sure all overridden methods share the same parameter names
      val (_, parentMethod) = overriddenMethods(overriddenMethods.size - 2)
      parentMethod.params.zip(methodDef.params).foreach { case (p1, p2) =>
        if (p1.name != p2.name)
          error(s"Overridden methods must use the same parameter names: Expected ${p2.name}, but got ${p1.name}.", parentMethod)
      }

      //val (baseClassDef, baseMethodDef) = overriddenMethods(1)
      //resolveTarget(methodDef)((baseClassDef, baseMethodDef))
    }

    methodDef.params.groupBy(_.name).foreach { case (_, cs) =>
      if (cs.size > 1)
        error(s"Ambiguous parameter names in method '${methodDef.name}'", cs: _*)
    }

    methodDef.params.foreach { p =>
      typecheck(p.typ)
      bindVar(p.name, p, p.typ, immutable = true)
    }

    /*if (!methodDef.returnsUnit && optReturn.isEmpty)
      throw new IllegalStateException(s"Method ${classDef.name}.${methodDef.name} must call return")*/

    val clsTy = TName(classDef.name, classDef.tyVars.map(v => TName(v.name, Seq()))) // TODO: Support generics
    typecheck(clsTy)

    val thisVar = VarDeclare(Name("this"), Some(clsTy), None, true)
    bindVar(thisVar.name, thisVar, clsTy, true)

    typecheck(methodDef.outType)
    typecheck(methodDef.body, methodDef.outType)(Some(classDef))
  }

  def typecheck(constructorDef: ConstructorDef, classDef: ClassDef): Unit = scopedTypeContext {
    val overriddenConstructors = lookupConstructorCandidates(classDef, constructorDef.params.map(_.typ))

    resolveTarget(constructorDef)(classDef)
    //val (baseClassDef, baseConstructorDef) = overriddenConstructors.head
    //resolveTarget(constructorDef)((baseClassDef, baseConstructorDef))

    constructorDef.params.foreach { p =>
      typecheck(p.typ)
      bindVar(p.name, p, p.typ, immutable = true)
    }

    constructorDef.params.groupBy(_.name).foreach { case (_, cs) =>
      if (cs.size > 1)
        error(s"Ambiguous parameter names in constructor '${classDef.name}'", cs: _*)
    }

    constructorDef.body.foreach {
      case Return(expression) =>
        error(s"Constructor '${classDef.name}' must not contain a return statement", expression)
      case _ => // nothing
    }

    // Super call handling
    val (superCalls, indices) = constructorDef.body.zipWithIndex.flatMap {
      case (Expr(e: Super), idx) => Some((e, idx))
      case _ => None
    }.unzip

    val superCallIndex = indices.headOption.getOrElse(-1)
    if (superCalls.size > 1) {
      error(s"Constructor '${classDef.name}' must not contain more than one supercall", superCalls: _*)
    } else if (superCallIndex > 0) {
      error(s"Super must be called first in constructor '${classDef.name}'", superCalls: _*)
    }

    val beforeSuperBody = constructorDef.body.slice(0, superCallIndex + 1)
    val afterSuperBody = constructorDef.body.slice(superCallIndex + 1, constructorDef.body.size)

    val clsTy = TName(classDef.name, classDef.tyVars.map(v => TName(v.name, Seq())))
    typecheck(clsTy)

    typecheck(beforeSuperBody, clsTy)(Some(classDef))

    // bind this after the super call !
    val thisVar = VarDeclare(Name("this"), Some(clsTy), None, true)
    bindVar(thisVar.name, thisVar, clsTy, true)

    typecheck(afterSuperBody, clsTy, allowImmutableFieldAssignment = true)(Some(classDef))
  }

  /** Statements */

  def typecheck(statements: Seq[Statement], rt: Type, allowImmutableFieldAssignment: Boolean = false)(implicit classDef: Option[ClassDef]): Unit =
    statements.foreach(typecheck(_, rt, allowImmutableFieldAssignment))

  def typecheck(statement: Statement, rt: Type, allowImmutableFieldAssignment: Boolean)(implicit classDef: Option[ClassDef]): Unit = statement match {
    case Expr(expression) =>
      typecheck(expression)
    case Return(expression) =>
      val outTyp = typecheck(expression)
      assertSubtype(outTyp, rt, statement)
    case superStmt@Super(args) =>
      val parentRef = classDef.getOrElse(
        throw IllegalStateException("Missing ClassDef in current context!")
      )
      val foundMatchingConstructor = parentRef.parentCls.exists {
        case t: TName if !t.isBuiltIn =>
          lookupClass(t.name) match
            case Some(parentCls: ClassDef) =>
              lookupConstructor(parentCls, args.map(typecheck), statement) match
                case Some((classDef, constructorDef)) =>
                  resolveTarget(superStmt)((classDef, constructorDef))
                  true
                case _ => false
            case _ => false
        case _ => false
      }
      if (!foundMatchingConstructor)
        error("No matching constructor found for super call", superStmt)

    case If(cnd, thn, els) =>
      val cndTyp = typecheck(cnd)
      scopedTypeContext { typecheck(thn, rt) }
      scopedTypeContext { typecheck(els, rt) }
      assertSubtype(cndTyp, TBoolean, cnd)
    case varDecl@VarDeclare(name, annotatedType, None, immutable) =>
      error(s"Declaration of variable '$name' without a value is not allowed")
    case varDecl@VarDeclare(name, annotatedType, Some(expr), immutable) =>
      val inferredType = typecheck(expr)
      (annotatedType, inferredType) match
        case (Some(ty1), ty2) =>
          typecheck(ty1)
          bindVar(name, varDecl, ty1, immutable)
          assertSubtype(ty2, ty1, varDecl)
        case (None, ty) =>
          bindVar(name, varDecl, ty, immutable)
    case varAssig@Assign(lhs@Var(name), rhs) =>
      val expTyp = typecheck(rhs)
      lookupVar(name) match
        case Some((target, typ, true)) =>
          error(s"Cannot assign immutable variable '$name'", statement)
        case Some((target, typ, false)) =>
          resolveTarget(lhs)(target)
          assertSubtype(expTyp, typ, statement)
        case None =>
          error(s"Can not assign to unbound variable '$name'", statement)
    case fieldAssign@Assign(select@Select(recv, targetName), rhs) =>
      val typ = typecheck(rhs)
      typecheck(recv) match {
        case TTuple(ts) =>
          error(s"Can not assign tuple elements", statement)
        case recvTy: TName if !recvTy.isBuiltIn =>
          recvTy.target match
            case Some(cls: ClassDef) => lookupField(cls, targetName, fieldAssign) match
              case Some((clazz, field)) =>
                if (!allowImmutableFieldAssignment && field.immutable)
                  error(s"Cannot assign to immutable field '$targetName'", statement)
                if (field.immutable && !uninitializedFields.contains(targetName))
                  error(s"Field '$targetName' is already initialized", statement)
                resolveTarget(select)((clazz, field))
                assertSubtype(typ, field.typ, fieldAssign)
                uninitializedFields -= targetName
              case _ => // Nothing
            case Some(trg) =>
              error(s"Unexpected receiver target $trg of type $recvTy", recv, statement)
            case _ =>
              error(s"Unresolved target for receiver $recv", recv, statement)
        case ty =>
          error(s"Unexpected receiver target '$recv' of type '$ty'", recv, statement)
      }
    case Assign(lhs, rhs) =>
      error(s"Can not assign term $lhs", statement)
    case phiStmt@VarPhiAssign(name, typ, ifStmt, thnName, elsName) =>
      scopedTypeContext {
        typecheck(ifStmt.thn, TAny)
        if (lookupVar(thnName).isEmpty)
          error(s"Name $thnName is not defined for VarPhiAssign", phiStmt)
      }
      scopedTypeContext {
        typecheck(ifStmt.els, TAny)
        if (lookupVar(elsName).isEmpty)
          error(s"Name $elsName is not defined for VarPhiAssign", phiStmt)
      }
      bindVar(name, phiStmt, typ, immutable = true)
  }

  /** Expressions */

  def typecheck(expression: Expression)(implicit classDef: Option[ClassDef]): Type = assignType(expression)(typecheckInternal(expression))

  def typecheckInternal(expression: Expression)(implicit classDef: Option[ClassDef]): Type = expression match {
    case IntLit(i) => TInt
    case DoubleLit(i) => TDouble
    case BoolLit(b) => TBoolean
    case StringLit(s) => TString
    case NullLit() => TNull
    case Tuple(exps) => TTuple(exps.map(typecheck))
    case UnOp("-", e) =>
      val eTy = typecheck(e)
      if (!subtype(eTy, TInt) && !subtype(eTy, TDouble))
        error(s"Required numeric type, but got $eTy", e)
      eTy
    case UnOp("!", e) =>
      val eTy = typecheck(e)
      if (!subtype(eTy, TBoolean))
        error(s"Required boolean type, but got $eTy", e)
      eTy
    case BinOp(e1, op, e2) =>
      op match
        case "==" | "!=" =>
          typecheck(e1)
          typecheck(e2)
          TBoolean
        case "+" =>
          val t1 = typecheck(e1)
          val t2 = typecheck(e2)
          if (subtype(t1, TString) && subtype(t2, TString))
            TString
          else if (subtype(t1, TInt) && subtype(t2, TInt))
            TInt
          else if (subtype(t2, TDouble) && subtype(t2, TDouble))
            TDouble
          else {
            error(s"Operator $op requires arguments of the same numeric or string type, but got $t1 and $t2", e1, e2)
            TDouble
          }
        case "-" | "*" | "/" =>
          val t1 = typecheck(e1)
          val t2 = typecheck(e2)
          if (subtype(t1, TInt) && subtype(t2, TInt))
            TInt
          else if (subtype(t2, TDouble) && subtype(t2, TDouble))
            TDouble
          else {
            error(s"Numeric operator $op requires arguments of the same type, but got $t1 and $t2", e1, e2)
            TDouble
          }
        case "%" =>
          val t1 = typecheck(e1)
          val t2 = typecheck(e2)
          if (subtype(t1, TInt) && subtype(t2, TInt))
            TInt
          else {
            error(s"Numeric operator $op requires Int arguments, but got $t1 and $t2", e1, e2)
            TInt
          }
        case ">" | ">=" | "<=" | "<" =>
          val t1 = typecheck(e1)
          val t2 = typecheck(e2)
          if (subtype(t1, TInt) && subtype(t2, TInt))
            TBoolean
          else if (subtype(t2, TDouble) && subtype(t2, TDouble))
            TBoolean
          else {
            error(s"Comparator $op requires arguments of the same type, but got $t1 and $t2", e1, e2)
            TBoolean
          }
        case "&&" | "||" =>
          val t1 = typecheck(e1)
          val t2 = typecheck(e2)
          if (subtype(t1, TBoolean) && subtype(t2, TBoolean))
            TBoolean
          else {
            error(s"Boolean operator $op requires Boolean arguments, but got $t1 and $t2", e1, e2)
            TInt
          }
        case "++" | "&" =>
          val t1 = typecheck(e1)
          val t2 = typecheck(e2)
          if (subtype(t1, TSet(TAny)) && subtype(t2, TSet(TAny))) {
            if (op == "++") join(t1, t2) else meet(t1, t2)
          } else {
            error(s"Set operator $op requires Set arguments, but got $t1 and $t2", e1, e2)
            TInt
          }
        case _ =>
          error(s"Unknown operator $op", expression)
          TAny

    case varRead@Var(name) => lookupVar(name) match
      case Some((target, typ, _)) =>
        resolveTarget(varRead)(target)
        typ
      case None =>
        error(s"Unbound variable '$name'", varRead)
        TAny

    case read@Select(recv, targetName) =>
      val recvTy = typecheck(recv)
      recvTy match
        case TTuple(ts) => // project
          val index = ParseUtil.parseTupleIndex(targetName) match
            case Some(idx) => idx - 1
            case _ =>
              error(s"Illegal tuple index $targetName", read)
              -1
          if (index < 0 || index > ts.size) {
            error(s"Index out of bounds: $index for Tuple size: ${ts.size}", recv)
            TAny
          } else
            ts(index)
        case t: TName => // field read
          t.target match
            case Some(cls: ClassDef) =>
              lookupField(cls, targetName) match
                case Some((c: ClassDef, f: FieldDef)) =>
                  resolveTarget(read)((c, f))
                  f.typ
                case _ => TAny
            case _ => TAny
        case _ => TAny

    case TypeCast(recv, toTyp) =>
      typecheck(recv)
      typecheck(toTyp)
      toTyp match {
        case t: TName if t.target.exists(_.isInstanceOf[ClassDef]) => // nothing
        case _ => error("Can only type cast to class type", expression)
      }
      toTyp

    case InstanceOf(recv, ofTyp) =>
      typecheck(recv)
      typecheck(ofTyp)
      ofTyp match {
        case t: TName if t.target.exists(_.isInstanceOf[ClassDef]) => // nothing
        case _ => error("Can only check type against class types", expression)
      }
      TBoolean

    case SetExp(exps, tty) =>
      val typs = exps.map(typecheck)
      if (typs.isEmpty && tty.isEmpty) {
        error("Empty set requires an explicit type", expression)
        TSet(TAny)
      } else
        TSet(tty.getOrElse(join(typs)))

    case setMember@SetMember(name, target, predicate) =>
      typecheck(target) match {
        case TSet(ty) =>
          bindVar(name, setMember, ty, immutable = true)
          if (predicate.isDefined)
            assertSubtype(typecheck(predicate.get), TBoolean, target)
          ty
        case _ =>
          error("Expects set type for member test", expression)
          bindVar(name, setMember, TAny, immutable = true)
          TAny
      }

    case SetComprehension(member, body) =>
      scopedTypeContext {
        member.foreach(typecheck)
        val bodyTy = typecheck(body)
        TSet(bodyTy)
      }

    case methodCallExpr@MethodCall(recv, fun, tyArgs, args, isFix) =>
      typecheck(recv) match
        case t: TName =>
          t.target match
            case Some(cls: ClassDef) => lookupMethod(cls, fun, args.map(typecheck)) match
              case None => TAny
              case Some((clsDef, methodDef)) =>
                resolveTarget(methodCallExpr)((clsDef, methodDef))
                typecheck(methodDef.outType)
                methodDef.outType
            case _ => TAny
        case typ =>
          error(s"Can not lookup method '$fun' for expression of type '$typ'", expression)
          TAny

    case constrCall@ConstructorCall(name, tyArgs, args) =>
      lookupClass(name) match
        case Some(cls: ClassDef) =>
          // Ensure there is only one constructor
          val constructors = cls.constructors
          if (constructors.size > 1)
            error(s"Ambiguous constructor for class $name", expression)
          else if (constructors.isEmpty)
            error(s"Missing constructor for class $name", expression)
          val primaryConstructor = constructors.head
          val expectedNumArgs = primaryConstructor.params.size
          // Ensure the number of type arguments matches
          if (cls.tyVars.size != tyArgs.size)
            error(s"Expected ${cls.tyVars.size} type arguments, but got ${tyArgs.size}", expression)
          // Ensure the number of arguments matches
          if (args.size != expectedNumArgs)
            error(s"Expected $expectedNumArgs arguments, but got ${args.size}", expression)
          val argTys = args.map(typecheck)
          // Ensure the argument types match
          primaryConstructor.params.zip(argTys).foreach { case (p@Param(_, ty), argTy) =>
            typecheck(ty)
            assertSubtype(argTy, ty, expression)
          }

          resolveTarget(constrCall)((cls, primaryConstructor))

          val clsTy = TName(cls.name, tyArgs)
          typecheck(clsTy)
          clsTy
        case None => TAny
  }

  /** Types */

  def typecheck(typ: Type): Unit = typ match {
    case TTuple(tys) => tys.foreach(typecheck)
    case TSet(ty) => typecheck(ty)
    case TAny | TNull => // nothing
    case t: TName if t.isBuiltIn => // nothing
    case t: TName => resolveNamedType(t)
  }

  /** Resolve targets */

  private def resolveTarget[T](term: Resolvable[T] with SourceLocation)(computeTarget: => T): T = {
    val newTarget = computeTarget
    term.resolved(newTarget, force = true)
    newTarget
  }

  private def resolveNamedType(ty: Type): Option[ClassDef] = {
    ty match
      case t: TName if t.isBuiltIn =>
        None
      case t@TName(name, tys) =>
        // TODO: Support generics
        lookupClass(name) match
          case Some(classDef) =>
            resolveTarget(t)(classDef)
            // Resolve nested generics
            tys.foreach(resolveNamedType)
            Some(classDef)
          case None => None
      case _ => None
  }
