package inca.frontend.objectoriented.typechecker;

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._
import inca.frontend.util.{Resolvable, Typeable}

trait Typechecker extends TypeContext with TypeIO with ScalaTypeContext {
  def typecheck(program: Seq[Module]): Unit = scopedTypeContext {
    program.foreach(bindModule)
    program.foreach(typecheck)
  }

  /*
   * Module
   */

  def typecheck(module: Module): Unit = scopedTypeContext {
    for (imp <- module.imports; importedModule <- lookupModule(imp.name)) {
      resolveTarget(imp)(importedModule)

      for (clazz <- importedModule.classes if !clazz.vis.contains(Private))
        bindClass(clazz, importedModule)
    }

    // bind symbols first
    module.classes.foreach(bindClass(_, module))

    // resolve all parent class refs before typechecking any and thereby check the inheritance
    module.classes.foreach(_.parentClassRefs.foreach(lookupClassRef))

    // type scala top-level definitions
    typecheckTopLevelObject()

    module.classes.foreach(typecheck)
  }

  def typecheck(classDef: ClassDef): Unit = {
    classDef.contentMap.foreach {
      case (_, cs: Seq[ConstructorDef]) => // nothing
      case (_, cs) if cs.size > 1 =>
        error(s"Ambiguous names in class ${classDef.name}", cs:_*)
    }

    classDef.content.foreach {
      case field: FieldDef => typecheck(field)
      case method: MethodDef => typecheck(method, classDef)
      case constructor: ConstructorDef => typecheck(constructor, classDef)
    }
  }

  def typecheck(fieldDef: FieldDef): Unit = {
    typecheck(fieldDef.typ)

    fieldDef.body match {
      case Some(expr) =>
        val expTyp = typecheck(expr)
        assertSubtype(expTyp, fieldDef.typ, fieldDef)
      case None => // nothing
    }
  }

  def typecheck(methodDef: MethodDef, classDef: ClassDef): Unit = scopedTypeContext {
    methodDef.params.foreach { p =>
      typecheck(p.typ)
      bindVar(p.name, p, p.typ, immutable = true)
    }
    methodDef.params.groupBy(_.name).foreach { case (_, cs) =>
      if (cs.size > 1)
        error(s"Ambiguous parameter names in method ${methodDef.name}", cs: _*)
    }
    methodDef.body.foreach {
      case ExprStmt(expression) => expression match {
        case SuperExpr(_) =>
          error(s"Method ${methodDef.name} must not contain a super constructor call.", expression)
        case _ => // nothing
      }
      case _ => // nothing
    }

    bindVar(Name("this"), classDef, classDef.typ, immutable = true)

    typecheck(methodDef.outType)

    typecheck(methodDef.body, methodDef.outType)
  }

  def typecheck(constructorDef: ConstructorDef, classDef: ClassDef): Unit = scopedTypeContext {
    if (constructorDef.annos.contains(MainAnnotation))
      error(s"Constructor ${classDef.name} can not be a main method.", constructorDef)

    constructorDef.params.foreach { p =>
      typecheck(p.typ)
      bindVar(p.name, p, p.typ, immutable = true)
    }
    constructorDef.params.groupBy(_.name).foreach { case (_, cs) =>
      if (cs.size > 1)
        error(s"Ambiguous parameter names in constructor ${classDef.name}", cs: _*)
    }
    constructorDef.body.foreach {
      case ReturnStmt(expression) =>
        error(s"Constructor ${classDef.name} must not contain a return statement.", expression)
      case _ => // nothing
    }

    val superCalls = constructorDef.body.filter {
      case ExprStmt(expression) => expression match {
        case SuperExpr(_) => true
        case _ => false
      }
      case _ => false
    }
    if (superCalls.size > 1) {
      error(s"Constructor ${classDef.name} must not contain more than one supercall.", superCalls:_*)
    }

    bindVar(Name("this"), classDef, classDef.typ, immutable = true)

    typecheck(constructorDef.body, classDef.typ, allowImmutableFieldAssignment = true)
  }

  def typecheck(typ: Type): Unit = typ match {
    case TTuple(tys) => tys.foreach((t: Type) => typecheck(t))
    case TClass(ref) => lookupClassRef(ref)
    case TAny => // nothing
    case TNull => // nothing
    case TScalaInt | TScalaBoolean | TScalaAny | TScalaDouble | TScalaLong | TScala(_) => // nothing
    case _ => throw new IllegalArgumentException(s"Currently does not support $typ")
  }

  def typecheck(statements: Seq[Statement], rt: Type, allowImmutableFieldAssignment: Boolean = false): Unit =
    statements.foreach(typecheck(_, rt, allowImmutableFieldAssignment))

  def typecheck(statement: Statement, rt: Type, allowImmutableFieldAssignment: Boolean): Unit = statement match {
    case ExprStmt(expression) => typecheck(expression)
    case ReturnStmt(expression) =>
      val outTyp = typecheck(expression)
      assertSubtype(outTyp, rt, statement)
    case fieldAssignStmt@FieldAssignStmt(recv, name, expression) =>
      val typ = typecheck(expression)
      typecheck(recv) match {
        case TClass(ref) => lookupField(lookupClassRef(ref), name) match {
          case Some((clazz, field)) =>
            if (!allowImmutableFieldAssignment && field.immutable)
              error(s"Can not assign to immutable field ${field.name}", statement)
            resolveTarget(fieldAssignStmt)((clazz, field))
            assertSubtype(typ, field.typ, expression)
          case None => // Nothing
        }
        case typ => error(s"Can not lookup field $name for expression of type $typ", statement)
      }
    case varDeclareStmt@VarDeclareStmt(name, typ, expression, immutable) =>
      typecheck(typ)
      bindVar(name, varDeclareStmt, typ, immutable)
      expression.foreach { exp =>
        val expTyp = typecheck(exp)
        assertSubtype(expTyp, typ, varDeclareStmt)
      }
    case varAssignStm@VarAssignStmt(targetName, expression) =>
      val expTyp = typecheck(expression)
      lookupVar(targetName) match {
        case Some((target, typ, immutable)) =>
          if (immutable) {
            error(s"Cannot assign to immutable variable $targetName", statement)
          } else {
            resolveTarget(varAssignStm)(target)
            assertSubtype(expTyp, typ, statement)
          }
        case None => // nothing
      }
    case IfStmt(cnd, thn, els) =>
      val cndTyp = typecheck(cnd)
      typecheck(thn, rt)
      typecheck(els, rt)

      assertSubtype(cndTyp, TScalaBoolean, cnd)
    case phiStmt@VarPhiAssignStmt(name, typ, ifStmt, thnName, elsName) =>
      bindVar(name, phiStmt, typ, immutable = true)
  }

  def assertSubtype(ty1: Type, ty2: Type, loc: SourceLocation): Unit = {
    if (!subtype(ty1, ty2))
      error(s"Expected $ty2, but got $ty1", loc)
  }

  def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match {
    case (_, TAny) => true
    case (TNull, TNull) => true
    case (TNull, TClass(_)) => true
    case (TClass(ref1), TClass(ref2)) if ref1 == ref2 => true
    case (TClass(ref1), TClass(_)) =>
      val parents = lookupClassRef(ref1).get.parentClassRefs
      parents.exists { parent => if (parent.target.isDefined) subtype(parent.target.get.typ, ty2) else false }
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      tys1.zip(tys2).forall(tt => subtype(tt._1, tt._2))
    case (TScala(s1), TScala(s2)) =>
      subtypeScala(s1.tree, s2.tree)
    case _ => false
  }

  def assignType(term: Typeable[Type] with SourceLocation)(computeType: => Type): Type = {
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
        annotated
      case None =>
        term.typed(inferred)
        inferred
    }
  }

  final def typecheck(expression: Expression): Type = assignType(expression)(typecheckInternal(expression))

  def typecheckInternal(expression: Expression): Type = expression match {
    case NullExpr() =>
      TNull
    case superExpr@SuperExpr(args) =>
      lookupVar(Name("this")) match {
        case Some((_, TClass(ref), _)) =>
          // `this` classRef will always be resolved at this point
          val clazz = ref.target.get
          val parentRef = clazz.parentClassRefs.headOption
          if (parentRef.isEmpty) {
            error(s"Missing parent class for class ${clazz.name}", expression)
            TAny
          } else {
            // classRef of parent will be resolved, but might still be invalid e.g. extend from a class that does not
            // exist
            lookupConstructor(parentRef.get.target, args.size, expression) match {
              case Some((classDef, constructorDef)) =>
                if (constructorDef.params.size != args.size) {
                  error(s"Expected ${constructorDef.params.size} arguments but got ${args.size} arguments", expression)
                } else {
                  constructorDef.params.zip(args).foreach { case (param, arg) =>
                    assertSubtype(typecheck(arg), param.typ, arg)
                  }
                }
                resolveTarget(superExpr)((classDef, constructorDef))
                TUnit
              case None =>
                TAny
            }
          }
        case None =>
          TAny
      }
    case varRead@VarReadExpr(targetName) =>
      lookupVar(targetName) match {
        case Some((target, typ, _)) =>
          resolveTarget(varRead)(target)
          typ
        case None => TAny
      }
    case fieldReadExpr@FieldReadExpr(recv, targetName) =>
      typecheck(recv) match {
        case TClass(ref) => lookupField(lookupClassRef(ref), targetName) match {
          case Some((clazz, field)) =>
            resolveTarget(fieldReadExpr)((clazz, field))
            field.typ
          case None => TAny
        }
        case typ =>
          error(s"Can not lookup field $targetName for expression of type $typ", recv)
          TAny
      }
    case construtorExpr@ConstructorExpr(className, args) =>
      lookupClassRef(className) match {
        case None => TAny
        case classDefOption@Some(classDef) =>
          lookupConstructor(classDefOption, args.size, expression) match {
            case None => classDef.typ
            case Some((_, constructorDef)) =>
              if (constructorDef.params.size != args.size) {
                error(s"Expected ${constructorDef.params.size} arguments but got ${args.size} arguments", expression)
              }
              constructorDef.params.zip(args).foreach { case (param, arg) =>
                assertSubtype(typecheck(arg), param.typ, arg)
              }
              resolveTarget(construtorExpr)(constructorDef)
              classDef.typ
          }
      }
    case methodCallExpr@MethodCallExpr(recv, fun, args) =>
      typecheck(recv) match {
        case TClass(ref) =>
          // We can call methods on instances of classes we might no have yet resolved
          lookupMethod(lookupClassRef(ref), fun) match {
          case None => TAny
          case Some(methodDef) =>
            if (methodDef.params.size != args.size) {
              error(s"Expected ${methodDef.params.size} arguments but got ${args.size} arguments", expression)
            }
            methodDef.params.zip(args).foreach { case (param, arg) =>
              val argTyp = typecheck(arg)
              assertSubtype(argTyp, param.typ, arg)
            }
            resolveTarget(methodCallExpr)(methodDef)
            methodDef.outType
        }
        case typ =>
          error(s"Can not lookup method $fun for expression of type $typ", expression)
          TAny
      }
    case TypeCastExpr(recv, toTyp) =>
      typecheck(recv)
      typecheck(toTyp)
      toTyp

    case InstanceOfExpr(recv, ofTyp) =>
      typecheck(recv)
      typecheck(ofTyp)
      TScalaBoolean

    case EqualsExpr(obj1, obj2) =>
      typecheck(obj1)
      typecheck(obj2)
      TScalaBoolean

    case TupleExpr(exps) =>
      TTuple(exps.map(typecheck))

    case TupleReadExpr(recv, index) =>
      typecheck(recv) match {
        case TTuple(ts) if index.raw <= 0 || index.raw > ts.size =>
          error(s"Index out of bounds: $index for Tuple size: ${ts.size}", recv)
          TAny
        case TTuple(ts) =>
          ts(index.raw-1)
        case ty =>
          error(s"Expected Tuple, but got $ty", recv)
          TAny
      }


    case BaseLitExpr(code) =>
      typecheckDecodeScala(code.syntax, expression)

    case BaseApplyExpr(fun, args) =>
      import meta._
      val argTys = args.zipWithIndex.map { case (a, ix) =>
        ("param$_" + ix, typecheck(a))
      }
      val paramString = argTys.map { case (name, ty) =>
        q"val ${Pat.Var(Term.Name(name))}: ${ty.asScala} = Predef.???".syntax
      }.mkString(";\n")

      val codeArgs = argTys.map(a => Term.Name(a._1))
      val codeSource = s"{$paramString;\n${fun.syntax}(${codeArgs.mkString(", ")})}"
      typecheckDecodeScala(codeSource, expression)

    case BaseApplyUnaryExpr(op, exp) =>
      import meta._
      val (expName, expTy) = (Term.Name("param$_exp"), typecheck(exp))
      val paramString = q"val ${Pat.Var(expName)}: ${expTy.asScala} = Predef.???".syntax
      val codeSource = s"{$paramString;\n${op.tree}$expName}"
      typecheckDecodeScala(codeSource, exp)

    case BaseApplyMethodExpr(recv, method, args) =>
      import meta._
      val recvTy = typecheck(recv)
      val recvString = q"val ${Pat.Var(Term.Name(recv.prettyprint("")))}: ${recvTy.asScala} = Predef.???".syntax

      val codeSource =
        if (args.isEmpty) s"{$recvString;\n${recv.prettyprint("")}.$method}" else {
          val argTys = args.getOrElse(Seq()).zipWithIndex.map { case (a, ix) =>
            ("param$_" + ix, typecheck(a))
          }

          val paramString = argTys.map { case (name, ty) =>
            q"val ${Pat.Var(Term.Name(name))}: ${ty.asScala} = Predef.???".syntax
          }.mkString(";\n")
          val codeArgs = argTys.map(a => Term.Name(a._1))
          s"{$paramString;\n$recvString;\n${recv.prettyprint("")}.$method(${codeArgs.mkString(", ")})}"
        }
      typecheckDecodeScala(codeSource, expression)

    case BaseApplyInfixExpr(left, op, right) =>
      import meta._
      val (leftName, leftTy) = (Term.Name("param$_left"), typecheck(left))
      val (rightName, rightTy) = (Term.Name("param$_right"), typecheck(right))

      (leftTy, op.tree.value, rightTy) match {
        /*case (TSet(tyl), "++", TSet(tyr)) =>
          TSet(join(tyl, tyr))
        case (TSet(tyl), "&", TSet(tyr)) =>
          TSet(join(tyl, tyr))*/
        case _ =>
          val paramString = Seq(
            q"val ${Pat.Var(leftName)}: ${leftTy.asScala} = Predef.???".syntax,
            q"val ${Pat.Var(rightName)}: ${rightTy.asScala} = Predef.???".syntax).mkString("\n")

          val codeSource = s"{$paramString;\n$leftName ${op.tree} $rightName}"
          typecheckDecodeScala(codeSource, expression)
      }
  }

  def typecheckDecodeScala(codeSource: String, loc: SourceLocation): Type = {
    typecheckScala(codeSource) match {
      case Left(typ) =>
        TypeHelper.decode(typ) match {
          case Right(ty) => ty
          case Left(msg) =>
            error(msg, loc)
            TAny
        }
      case Right(err) =>
        error(err.getMessage, loc)
        TAny
    }
  }

  def lookupClassRef(classRef: ClassRef): Option[ClassDef] = {
    lookupClass(classRef.name) match {
      case Some(classDef) =>
        resolveTarget(classRef)(classDef)
        Some(classDef)
      case None => None
    }
  }

  def resolveTarget[T](term: Resolvable[T] with SourceLocation)(computeTarget: => T): T = {
    val newTarget = computeTarget
    term.target match {
      case Some(oldTarget) =>
        if (oldTarget != newTarget) {
          throw new RuntimeException("Fuck this shit I'm out !")
          error(s"Resolved $term to new target $newTarget, which differs from previously computed target $oldTarget", term)
        }
        oldTarget
      case None =>
        term.resolved(newTarget)
        newTarget
    }
  }
}
