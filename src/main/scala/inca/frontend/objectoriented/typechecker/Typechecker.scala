package inca.frontend.objectoriented.typechecker;

import inca.backend.analyze.Graph
import inca.compiler.SourceLocation
import inca.frontend.objectoriented.core._
import inca.frontend.util.{Resolvable, Typeable}

import java.util.UUID
import scala.collection.immutable.MultiDict
import scala.util.hashing.MurmurHash3

private case class InheritanceGraph(classes: Seq[ClassDef]) extends Graph[ClassDef, Option[String]] {
  private val clsMap: Map[Name, ClassDef] = classes.map(c => c.name -> c).toMap

  classes.foreach { cls =>
    addNode(cls)
    cls.parentClassRefs.map { p =>
      addEdge(cls, clsMap(p.name), None)
    }
  }

  override protected def nodeToGraphViz(n: ClassDef): String = n.name.raw
  override protected def edgeGraphVizAttributes(from: ClassDef, to: ClassDef, info: Option[String]): String = ""
  override protected def nodeGraphVizAttributes(from: ClassDef): String = ""
}

trait Typechecker extends TypeContext with TypeIO with ScalaTypeContext {
  def typecheck(program: Seq[Module]): Unit = scopedTypeContext {
    program.foreach(bindModule)
    program.foreach(typecheck)
  }


  /*
   * Module
   */

  def typecheck(module: Module): Unit = scopedTypeContext {
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
    val inheritanceGraph = InheritanceGraph(module.classes)
    inheritanceGraph.cycles.foreach { c =>
      error(s"Cycle in inheritance hierarchy: ${c.map(_.name.raw).mkString(" <- ")}", c: _*)
    }
    // the rest of the typechecker is not working with cyclic ClassRefs
    if (inheritanceGraph.cycles.nonEmpty)
      return

    // bind symbols first
    module.classes.foreach(bindClass(_, module))
    // resolve all parent class refs before typechecking any and thereby check the inheritance
    module.classes.foreach(_.parentClassRefs.foreach(lookupClassRef))
    // type scala top-level definitions
    typecheckTopLevelObject()

    module.classes.foreach(typecheck)
  }

  var uninitializedFields: Map[Name, FieldDef] = Map()

  def typecheck(classDef: ClassDef): Unit = {
    classDef.contentMap.foreach {
      case (_, _: Seq[ConstructorDef]) => // nothing
      case (_, cs) if cs.size > 1 =>
        error(s"Ambiguous names in class `${classDef.name}`", cs:_*)
    }

    // make sure all fields are initialized after a constructor is executed
    uninitializedFields = Map()

    classDef.content.foreach {
      case field: FieldDef => typecheck(field, classDef)
      case method: MethodDef => typecheck(method, classDef)
      case constructor: ConstructorDef =>
        // every path trough a constructor must initialize all fields
        val storeUninitializedFields = uninitializedFields
        typecheck(constructor, classDef)
        uninitializedFields.foreach { case (fieldName, fieldDef) =>
          error(s"Field `$fieldName` is not initialized", fieldDef)
        }
        uninitializedFields = storeUninitializedFields
    }
  }

  def typecheck(fieldDef: FieldDef, classDef: ClassDef): Unit = {
    typecheck(fieldDef.typ)

    fieldDef.body match {
      case Some(expr) =>
        val expTyp = typecheck(expr)(classDef)
        assertSubtype(expTyp, fieldDef.typ, fieldDef)
      case None =>
        uninitializedFields += (fieldDef.name -> fieldDef)
    }

    fieldDef.aggregateMethod match {
      case Some((ref, methodName)) =>
        val clazz = lookupClassRef(ref)
        val ty = fieldDef.typ
        val method = lookupMethod(clazz, Seq(ty, ty), methodName)
        if (method.isDefined)
          resolveTarget(fieldDef)(method.get)
      case None => // nothing
    }
  }

  def typecheck(methodDef: MethodDef, classDef: ClassDef): Unit = scopedTypeContext {
    // get all overriden methods and assign them the same signature
    val overrideMethods = lookupMethodCandidates(Some(classDef), methodDef.params.map(_.typ), methodDef.name)
    resolveSignatures(overrideMethods.map(_._2))

    methodDef.params.foreach { p =>
      typecheck(p.typ)
      bindVar(p.name, p, p.typ, immutable = true)
    }
    methodDef.params.groupBy(_.name).foreach { case (_, cs) =>
      if (cs.size > 1)
        error(s"Ambiguous parameter names in method `${methodDef.name}`", cs: _*)
    }
    methodDef.body.foreach {
      case ExprStmt(expression) => expression match {
        case SuperExpr(_) =>
          error(s"Method `${methodDef.name}` must not contain a super constructor call", expression)
        case _ => // nothing
      }
      case _ => // nothing
    }

    /*if (!methodDef.returnsUnit && optReturn.isEmpty)
      throw new IllegalStateException(s"Method ${classDef.name}.${methodDef.name} must call return")*/

    // main method must not use this, since it is static
    if (!methodDef.isStatic)
      bindVar(Name("this"), classDef, classDef.typ, immutable = true)

    typecheck(methodDef.outType)

    typecheck(methodDef.body, methodDef.outType)(classDef)
  }

  def typecheck(constructorDef: ConstructorDef, classDef: ClassDef): Unit = scopedTypeContext {
    if (constructorDef.isStatic)
      error(s"Constructor `${classDef.name}` can not be a static method", constructorDef)

    val overrideConstructors = lookupConstructorCandidates(Some(classDef), constructorDef.params.map(_.typ))
    resolveSignatures(overrideConstructors.map(_._2))

    constructorDef.params.foreach { p =>
      p.typ match {
        case ty =>
          typecheck(p.typ)
          bindVar(p.name, p, ty, immutable = true)
      }
    }

    constructorDef.params.groupBy(_.name).foreach { case (_, cs) =>
      if (cs.size > 1)
        error(s"Ambiguous parameter names in constructor `${classDef.name}`", cs: _*)
    }
    constructorDef.body.foreach {
      case ReturnStmt(expression) =>
        error(s"Constructor `${classDef.name}` must not contain a return statement", expression)
      case _ => // nothing
    }

    // Super call handling
    val (superCalls, indices) = constructorDef.body.zipWithIndex.flatMap {
      case (ExprStmt(e@SuperExpr(_)), idx) => Some((e, idx))
      case _ => None
    }.unzip

    val superCallIndex = indices.headOption.getOrElse(-1)
    if (superCalls.size > 1) {
      error(s"Constructor `${classDef.name}` must not contain more than one supercall", superCalls:_*)
    } else if (superCallIndex > 0) {
      error(s"Super must be called first in constructor `${classDef.name}`", superCalls:_*)
    }

    val beforeSuperBody = constructorDef.body.slice(0, superCallIndex+1)
    val afterSuperBody = constructorDef.body.slice(superCallIndex+1, constructorDef.body.size)

    typecheck(beforeSuperBody, classDef.typ)(classDef)
    // bind this after the super call !
    bindVar(Name("this"), classDef, classDef.typ, immutable = true)
    typecheck(afterSuperBody, classDef.typ, allowImmutableFieldAssignment = true)(classDef)
  }

  def typecheck(typ: Type): Unit = typ match {
    case TTuple(tys) => tys.foreach(typecheck)
    case TSet(ty) => typecheck(ty)
    case TClass(ref) => lookupClassRef(ref)
    case TAny => // nothing
    case TNull => // nothing
    case TScalaInt | TScalaBoolean | TScalaAny | TScalaDouble | TScalaLong | TScala(_) => // nothing
    case _ => throw new IllegalArgumentException(s"Currently does not support `$typ`")
  }

  def typecheck(statements: Seq[Statement], rt: Type, allowImmutableFieldAssignment: Boolean = false)(implicit classDef: ClassDef): Unit =
    statements.foreach(typecheck(_, rt, allowImmutableFieldAssignment))

  def typecheck(statement: Statement, rt: Type, allowImmutableFieldAssignment: Boolean)(implicit classDef: ClassDef): Unit = statement match {
    case ExprStmt(expression) => typecheck(expression)
    case ReturnStmt(expression) =>
      val outTyp = typecheck(expression)
      assertSubtype(outTyp, rt, statement)
    case fieldAssignStmt@FieldAssignStmt(recv, name, expression, _) =>
      val typ = typecheck(expression)
      typecheck(recv) match {
        case TClass(ref) => lookupField(lookupClassRef(ref), name) match {
          case Some((clazz, field)) =>
            if (!allowImmutableFieldAssignment && field.immutable)
              error(s"Can not assign to immutable field `${field.name}`", statement)
            resolveTarget(fieldAssignStmt)((clazz, field))
            assertSubtype(typ, field.typ, expression)
            uninitializedFields -= field.name
          case None => // Nothing
        }
        case typ => error(s"Can not lookup field `$name` for expression of type `$typ`", statement)
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
            error(s"Cannot assign to immutable variable `$targetName`", statement)
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
      error(s"Expected `$ty2`, but got `$ty1`", loc)
  }

  def assignType(term: Typeable[Type] with SourceLocation)(computeType: => Type): Type = {
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated))
          error(s"Inferred type `$inferred`, but expected annotated type `$annotated`", term)
        annotated
      case None =>
        term.typed(inferred)
        inferred
    }
  }

  final def typecheck(expression: Expression)(implicit classDef: ClassDef): Type = assignType(expression)(typecheckInternal(expression))

  def typecheckInternal(expression: Expression)(implicit classDef: ClassDef): Type = expression match {
    case NullExpr() =>
      TNull
    case superExpr@SuperExpr(args) =>
      classDef.typ match {
        case TClass(ref) =>
          // `this` classRef will always be resolved at this point
          val clazz = ref.target.get
          val parentRef = clazz.parentClassRefs.headOption
          if (parentRef.isEmpty) {
            error(s"Missing parent class for class `${clazz.name}`", expression)
            TAny
          } else {
            // classRef of parent will be resolved, but might still be invalid e.g. extend from a class that does not
            // exist
            lookupConstructor(parentRef.get.target, args.map(typecheck), expression) match {
              case Some((classDef, constructorDef)) =>
                resolveTarget(superExpr)((classDef, constructorDef))
                TUnit
              case None =>
                TAny
            }
          }
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
          error(s"Can not lookup field `$targetName` for expression of type `$typ`", recv)
          TAny
      }
    case construtorExpr@ConstructorExpr(className, args) =>
      lookupClassRef(className) match {
        case None => TAny
        case classDefOption@Some(clazz) =>
          val argTypes = args.map(typecheck)
          lookupConstructor(classDefOption, argTypes, expression) match {
            case Some((cls, constructorDef)) if cls == clazz =>
              resolveTarget(construtorExpr)(constructorDef)
              clazz.typ
            // we do not allow inheritance of constructors
            case Some(_) =>
              error(s"No matching constructor found for class `${classDefOption.get.name}`: this(${argTypes.mkString(",")})", expression)
              clazz.typ
            case None =>
              clazz.typ
          }
      }
    case methodCallExpr@MethodCallExpr(recv, fun, args) =>
      typecheck(recv) match {
        case clazzTyp@TClass(ref) =>
          // We can call methods on instances of classes we might no have yet resolved
          lookupMethod(lookupClassRef(ref), args.map(typecheck), fun) match {
            case None =>
              TAny
            case Some(methodDef) =>
              // TODO: We might allow calling static methods in the future
              if (methodDef.isStatic)
                error(s"Can not call static method `${methodDef.name}` on instance of type `$clazzTyp`", expression)
              resolveTarget(methodCallExpr)(methodDef)
              methodDef.outType
        }
        case typ =>
          error(s"Can not lookup method `$fun` for expression of type `$typ`", expression)
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
          error(s"Expected Tuple, but got `$ty`", recv)
          TAny
      }

    case SetExpr(exps, tty) =>
      val typs = exps.map(typecheck)
      if (typs.isEmpty && tty.isEmpty) {
        error("Empty set requires an explicit type", expression)
        TSet(TAny)
      } else
        TSet(tty.getOrElse(upperTypeBound(typs)))

    case setMember@SetMemberExpr(name, target, predicate) =>
      typecheck(target) match {
        case TSet(ty) =>
          bindVar(name, setMember, ty, immutable = true)
          if (predicate.isDefined) {
            assertSubtype(typecheck(predicate.get), TScalaBoolean, target)
          }
          ty
        case _ =>
          error("Expects set type for member test", expression)
          bindVar(name, setMember, TAny, immutable = true)
          TAny
      }

    case SetComprehension(member, body) =>
      member.foreach(typecheck)
      TSet(typecheck(body))

    case setFold@SetFold(recv, projection, classRef, methodName, neutral) =>
      typecheck(recv) match {
        case TSet(tty) =>
          val isNestedTuple = tty match {
            case TTuple(ts) if ts.exists(_.isInstanceOf[TTuple]) => true
            case _ => false
          }

          if (isNestedTuple)
            error("Fold does not support nested tuples.", recv)

          val ty = tty.flatten(setFold.aggIndex)
          assertSubtype(typecheck(neutral), ty, neutral)

          val hasOneAgg = projection.count {
            case VarReadExpr(Name("#")) => true
            case _ => false
          }
          if (hasOneAgg != 1)
            error("A fold projection requires exactly one aggregation '#'", projection: _*)

          projection.zip(tty.flatten).foreach {
            case (VarReadExpr(Name("#") | Name("_")), expTy) => expTy
            case (expr, expTy) => assertSubtype(typecheck(expr), expTy, expr)
          }

          val methodDef = lookupMethod(lookupClass(classRef.name), Seq(ty, ty), methodName)
          if (methodDef.isDefined) {
            assertSubtype(methodDef.get.outType, ty, methodDef.get)
            resolveTarget(setFold)(methodDef.get)
          } else
            error(s"Fold method `${classRef.name}.${methodName}` not found!", expression)
          ty
        case ty =>
          error("Fold can only be performed on sets!", expression)
          ty
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
        case (TSet(tyl), "++", TSet(tyr)) =>
          TSet(join(tyl, tyr))
        case (TSet(tyl), "&", TSet(tyr)) =>
          TSet(join(tyl, tyr))
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
          error(s"Resolved `$term` to new target `$newTarget`, which differs from previously computed target `$oldTarget`", term)
        oldTarget
      case None =>
        term.resolved(newTarget)
        newTarget
    }
  }

  def resolveSignatures[T <: Resolvable[Int]](callables: Seq[T]): Unit = {
    // Get the signature of the top most implementation
    val signature = if (callables.nonEmpty) callables.head.hashCode() else 0
    callables.foreach(_.target = Some(signature))
  }
}
