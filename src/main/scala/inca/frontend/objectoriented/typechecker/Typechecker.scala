package inca.frontend.objectoriented.typechecker;

import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._
import inca.frontend.util.Resolvable
import meta.quasiquotes._

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

    // type scala top-level definitions
    typecheckTopLevelObject()

    module.classes.foreach(typecheck)
  }

  def typecheck(classDef: ClassDef): Unit = {
    // check inheritance
    classDef.parentClassNames.foreach(lookupClassRef)

    classDef.contentMap.foreach { case (_, cs) =>
      if (cs.size > 1)
        error(s"Ambiguous names in class ${classDef.name}", cs:_*)
    }

    classDef.content.foreach {
      case field: FieldDef => typecheck(field)
      case method: MethodDef => typecheck(method, classDef)
      case constructor: ConstructorDef => typecheck(constructor, classDef)
    }
  }

  def typecheck(fieldDef: FieldDef): Unit = {
    fieldDef.body.foreach(typecheck)
    typecheck(fieldDef.typ)
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

    bindVar(Name("this"), classDef, classDef.typ, immutable = true)

    typecheck(methodDef.outType)

    typecheck(methodDef.body, methodDef.outType)
  }

  def typecheck(constructorDef: ConstructorDef, classDef: ClassDef): Unit = scopedTypeContext {
    constructorDef.params.foreach { p =>
      typecheck(p.typ)
      bindVar(p.name, p, p.typ, immutable = true)
    }
    constructorDef.params.groupBy(_.name).foreach { case (_, cs) =>
      if (cs.size > 1)
        error(s"Ambiguous parameter names in constructor ${classDef.name}", cs: _*)
    }

    bindVar(Name("this"), classDef, classDef.typ, immutable = true)

    typecheck(constructorDef.body, classDef.typ)
  }

  def typecheck(typ: Type): Unit = typ match {
    case TTuple(tys) => tys.foreach((t: Type) => typecheck(t))
    case TClass(ref) => lookupClassRef(ref)
    case TAny => // nothing
    case TNull => // nothing
    case TScalaInt | TScalaBoolean | TScalaAny | TScalaDouble | TScalaLong | TScala(_) => // nothing
    case _ => throw new IllegalArgumentException(s"Currently does not support $typ")
  }

  def typecheck(statements: Seq[Statement], rt: Type): Unit =
    statements.foreach(typecheck(_, rt))

  def typecheck(statement: Statement, rt: Type): Unit = statement match {
    case ExprStmt(expression) => typecheck(expression)
    case ReturnStmt(expression) =>
      val outTyp = typecheck(expression)
      assertSubtype(outTyp, rt, statement)
    case FieldAssignStmt(recv, name, expression) =>
      val typ = typecheck(expression)
      typecheck(recv) match {
        case TClass(ref) => lookupField(ref.target.get, name) match {
          case Some(field) =>
            assertSubtype(typ, field.typ, expression)
          case None => // Nothing
        }
        case typ => error(s"Can not lookup field $name for expression of type $typ", statement)
      }
    case decl@VarDeclareStmt(name, typ, expression, immutable) =>
      typecheck(typ)
      bindVar(name, decl, typ, immutable)
      expression.foreach { exp =>
        val expTyp = typecheck(exp)
        assertSubtype(expTyp, typ, exp)
      }
    case VarAssignStmt(targetName, expression) =>
      val expTyp = typecheck(expression)
      lookupVar(targetName) match {
        case Some((_, typ, immutable)) =>
          if (immutable) {
            error(s"Cannot assign to immutable variable $targetName", statement)
          } else {
            assertSubtype(expTyp, typ, statement)
          }
        case None => // nothing
      }
    case IfStmt(cnd, thn, els) =>
      val cndTyp = typecheck(cnd)
      typecheck(thn, rt)
      typecheck(els, rt)

      assertSubtype(cndTyp, TScalaBoolean, cnd)
  }

  def assertSubtype(ty1: Type, ty2: Type, loc: SourceLocation): Unit = {
    if (!subtype(ty1, ty2))
      error(s"Expected $ty2, but got $ty1", loc)
  }

  def subtype(ty1: Type, ty2: Type): Boolean = (ty1, ty2) match {
    case (_, TAny) => true
    case (TNull, TClass(_)) => true
    case (TClass(name1), TClass(name2)) if name1 == name2 => true
    case (TClass(name1), TClass(_)) =>
      val parents = name1.target.get.parentClassNames
      parents.exists(parent => subtype(parent.target.get.typ, ty2))
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size =>
      tys1.zip(tys2).forall(tt => subtype(tt._1, tt._2))
    case (TScala(s1), TScala(s2)) =>
      subtypeScala(s1.tree, s2.tree)
    case _ => false
  }

  def typecheck(expression: Expression): Type = expression match {
    case NullExpr() =>
      TNull
    case SuperExpr(args) =>
      lookupVar(Name("this")) match {
        case Some((_, TClass(ref), _)) =>
          lookupClassRef(ref) match {
            case Some(clazz) =>
              val parentRef = clazz.parentClassNames.headOption
              if (parentRef.isEmpty) {
                error(s"Undefined super class for class ${clazz.name}", expression)
                TAny
              } else {
                val parentClassDef = lookupClassRef(parentRef.get)
                if (parentClassDef.isEmpty) {
                  TAny
                } else {
                  val constructorDef = lookupConstructor(parentClassDef.get, expression)
                  if (constructorDef.isEmpty) {
                    error(s"Can not lookup constructor ${parentClassDef.get.name}", expression)
                  } else if (constructorDef.get.params.size != args.size) {
                    error(s"Expected ${constructorDef.get.params.size} arguments but got ${args.size} arguments", expression)
                  } else {
                    constructorDef.get.params.zip(args).foreach { case (param, arg) =>
                      assertSubtype(typecheck(arg), param.typ, arg)
                    }
                  }
                  parentClassDef.get.typ
                }
              }
            case None => TAny
          }
        case _ => TAny
      }
    case varRead@VarReadExpr(targetName) =>
      lookupVar(targetName) match {
        case Some((target, typ, _)) =>
          resolveTarget(varRead)(target)
          typ
        case None => TAny
      }
    case FieldReadExpr(recv, targetName) =>
      typecheck(recv) match {
        case TClass(ref) => lookupField(ref.target.get, targetName) match {
          case Some(field) => field.typ
          case None => TAny
        }
        case typ =>
          error(s"Can not lookup field $targetName for expression of type $typ", expression)
          TAny
      }
    case ConstructorExpr(className, args) =>
      lookupClassRef(className) match {
        case None => TAny
        case Some(classDef) =>
          lookupConstructor(classDef, expression) match {
            case None => classDef.typ
            case Some(constructorDef) =>
              if (constructorDef.params.size != args.size) {
                error(s"Expected ${constructorDef.params.size} arguments but got ${args.size} arguments", expression)
              }
              constructorDef.params.zip(args).foreach { case (param, arg) =>
                assertSubtype(typecheck(arg), param.typ, arg)
              }
              classDef.typ
          }
      }
    case MethodCallExpr(recv, fun, args) =>
      typecheck(recv) match {
        case TClass(ref) => lookupMethod(ref.target.get, fun) match {
          case None => TAny
          case Some(methodDef) =>
            if (methodDef.params.size != args.size) {
              error(s"Expected ${methodDef.params.size} arguments but got ${args.size} arguments", expression)
            }
            methodDef.params.zip(args).foreach { case (param, arg) =>
              val argTyp = typecheck(arg)
              assertSubtype(argTyp, param.typ, arg)
            }
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
    case TupleExpr(exps) =>
      TTuple(exps.map(typecheck))

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
        if (oldTarget != newTarget)
          error(s"Resolved $term to new target $newTarget, which differs from previously computed target $oldTarget", term)
        oldTarget
      case None =>
        term.resolved(newTarget)
        newTarget
    }
  }
}
