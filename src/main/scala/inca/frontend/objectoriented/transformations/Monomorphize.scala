package inca.frontend.objectoriented.transformations
import inca.frontend.functional.core
import inca.frontend.functional.core.{TConstr, TNothing}
import inca.frontend.objectoriented.core.{ClassDef, MethodDef}
import inca.frontend.objectoriented.core._
import inca.frontend.functional.util.Collect
import inca.util.{Gensym, Scala, TupleOps}
import truechange.SortType

import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable


object Monomorphize {
  def transformModule(module: Module): Module = {
    new Monomorphize(module).transModule()
  }
}

class Monomorphize(val module: Module) extends ModuleLowering {
  private val gensym: Gensym = new Gensym(Iterable.empty)

  private val monomorphicClassDef = mutable.Map[Name, ClassDef]()
  private val monomorphicMethodDef = mutable.Map[Name, MethodDef]()

  private val polymorphicClassDef = mutable.Map[Name, ClassDef]()
  private val polymorphicMethodDef = mutable.Map[(Name,Name), MethodDef]()    // (className, methodName) -> MethodDef

  private val polymorphicCallSites = mutable.Map[Name, Seq[(Name, Seq[(Name, Seq[Type])])]]()
  // className -> Seq[MethodName, Seq[ParamName, Seq[Types]]]]


  private val polymorphicToMonomorphic = mutable.Map[(Name, Seq[Type]), Name]()

  // in this maps: class/method-name -> list of the used Types for the type-parameters
  // so that they can each be zipped later TODO ?
  var polymorphicClassDefWithConcreteTypes: Map[Name, mutable.ArrayBuffer[Seq[Type]]] = Map[Name, mutable.ArrayBuffer[Seq[Type]]]()
  var polymorphicMethodDefWithConcreteTypes: Map[(Name,Name), mutable.ArrayBuffer[Seq[Type]]] = Map[(Name,Name), mutable.ArrayBuffer[Seq[Type]]]()

  private var subst: mutable.Map[Name, Type] = mutable.Map[Name, Type]() // maps generic parameters to types

    private def collectTypeApplications(): Unit = {

      module.classes.foreach(clsDef => collectClassDef(clsDef))

      def collectClassDef(classDef: ClassDef): Unit = {
        // check whether classDef itself is generic
        if (classDef.isGeneric){
          polymorphicClassDef.put(classDef.name,classDef)
        }

        // check all content of the classDef
        classDef.methods.foreach(methodDef => collectMethodDef(classDef, methodDef))
        classDef.fields.foreach(f => f.body match{
          case Some(expr) => collectExpression(expr)(classDef)
          case None =>
        })
        classDef.constructors.foreach(constr => collectConstructor(constr)(classDef))
        /*
        val gpTypes: Seq[Type] = ??? // gesammelte Typen
        val genTypes = classDef.genericTypeParams.map(gp => (gp.name, gpTypes))

        Seq((classDef.name,genTypes))

         */

        println(polymorphicClassDefWithConcreteTypes)
        println(polymorphicMethodDefWithConcreteTypes)
      }


      def collectMethodDef(classDef: ClassDef, methodDef: MethodDef): Unit = {
        if (methodDef.isGeneric){
          polymorphicMethodDef.put((classDef.name,methodDef.name),methodDef)
        }

        methodDef.body.foreach(stm => collectStatement(stm)(classDef))
      }

      def collectConstructor(constructorDef: ConstructorDef)(implicit classDef: ClassDef): Unit = {
        constructorDef.body.foreach(stm => collectStatement(stm))
      }


      // Output Seq[(ClassName, Seq[GenParamName, Seq[Type]])]


      def collectExpression(exp: Expression)(implicit classDef: ClassDef): Unit = exp match {
        case VarReadExpr(targetName) =>
        case FieldReadExpr(recv, targetName) => collectExpression(recv)
        case ConstructorExpr(classRef, tyArgs, args) =>
          args.foreach(e => collectExpression(e))
          // check whether constructor belongs to generic class and if save types
          if (tyArgs.nonEmpty) {
            if (polymorphicClassDefWithConcreteTypes.keys.exists(_ == classRef.name)) {
              polymorphicClassDefWithConcreteTypes(classRef.name).append(tyArgs)
            }
            else {
              polymorphicClassDefWithConcreteTypes += (classRef.name -> mutable.ArrayBuffer(tyArgs))
            }
          }
          // TODO fix problem with inheritance chain where middle class is not in polymorphicClassDefWithConcreteTypes
          //    maybe problem here
          classDef.parentClassRefs.foreach { tname =>
            if (tname.tyArgs.nonEmpty) {
              val newSuperTyArgs = tname.tyArgs.map{ arg =>
                if (classDef.genericTypeParams.exists(p => TName(p.name) == arg)) {
                  val index = classDef.genericTypeParams.indexOf(classDef.genericTypeParams.filter(p => TName(p.name) == arg).head)
                  tyArgs(index)
                }
              }
              if (polymorphicClassDefWithConcreteTypes.keys.exists(_ == tname.name)) {
                polymorphicClassDefWithConcreteTypes(tname.name).append(tname.tyArgs)
              }
              else {
                polymorphicClassDefWithConcreteTypes += (tname.name -> mutable.ArrayBuffer(tname.tyArgs))
              }
            }
          }

        case SuperExpr(args) =>
          args.foreach(collectExpression(_))
        case MethodCallExpr(recv, funName, tyArgs, args, isFix) =>
          collectExpression(recv)
          if (tyArgs.nonEmpty){
            if (polymorphicMethodDefWithConcreteTypes.keys.exists(_ == (classDef.name,funName))) {
              polymorphicMethodDefWithConcreteTypes(classDef.name,funName).append(tyArgs)
            }
            else {
              polymorphicMethodDefWithConcreteTypes += ((classDef.name,funName) -> mutable.ArrayBuffer(tyArgs))
            }

          }
        case TypeCastExpr(recv, toTyp) => collectExpression(recv)
        case InstanceOfExpr(recv, ofTyp) => collectExpression(recv)
        case NullExpr() =>
        case TupleReadExpr(recv, index) => collectExpression(recv)
        case TupleExpr(exps) => exps.foreach(collectExpression(_))
        case SetExpr(exps, tty) => exps.foreach(collectExpression(_))
        case SetMemberExpr(name, recv, predicate) =>
          collectExpression(recv)
          predicate match {
            case Some(e) => collectExpression(e)
            case None =>
          }
        case SetComprehension(member, body) =>
          member.foreach(collectExpression(_))
          collectExpression(body)
        case SetFold(recv, projection, opClass, opMethod, neutral) =>
          collectExpression(recv)
          projection.foreach(collectExpression(_))
          collectExpression(neutral)
        case SetFromEdb(edbName, tty) =>
        case BaseLitExpr(code) =>
        case BaseApplyExpr(fun, args) =>
          args.foreach(collectExpression(_))
        case BaseApplyInfixExpr(left, op, right) =>
          collectExpression(left)
          collectExpression(right)
        case BaseApplyMethodExpr(recv, method, args) =>
          collectExpression(recv)
          args match {
            case Some(argSeq) => argSeq.foreach(collectExpression(_))
            case None =>
          }
        case BaseApplyUnaryExpr(op, exp) => collectExpression(exp)
      }


      def collectStatement(stm: Statement)(implicit classDef: ClassDef): Unit = {
        stm match {
          case ExprStmt(expression) => collectExpression(expression)
          case ReturnStmt(expression) => collectExpression(expression)
          case FieldAssignStmt(recv, name, expression) =>
            collectExpression(recv)
            collectExpression(expression)
          case VarDeclareStmt(name, typ, maybeExpression, immutable) => maybeExpression match {
            case Some(expr) => collectExpression(expr)
            case None =>
          }
          case VarAssignStmt(targetName, expression) => collectExpression(expression)
          case VarPhiAssignStmt(name, typ, ifStmt, thnName, elsName) =>
            collectStatement(ifStmt)
          case IfStmt(cnd, thn, els) =>
            collectExpression(cnd)
            thn.foreach(stm => collectStatement(stm))
            els.foreach(stm => collectStatement(stm))
        }
      }

    }


/*
  private def isMonomorphic(ty: Type): Boolean = ty match {
    case TAny => true
    case TNull => true
    case TTuple(ts) => ts.forall(isMonomorphic)
    case tn@TName(_) => tn.target match {
      case Some(ClassDef(_, _, _, Seq(), _, _)) => true
      case Some(ClassDef(_, _, _, _, _, _)) => false
      case None => true
    }
    case TClass(ty) => isMonomorphic(ty)
    case TScala(_) => true
    case TSet(ty) => isMonomorphic(ty)
  }

  override def transExpressionInternal(expression: Expression): Seq[Expression] = super.transExpressionInternal(expression)


  */
  override private[transformations] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    // make sure the class name is unique
    gensym.register(module.usedModuleNames.map(_.raw))
    gensym.register(classes.map(_.name.raw))
    collectTypeApplications()
    // generate names of monomorphic versions
    generateMonomorphicVersionNames()
    // TODO generate monomorphic versions of ClassDefs & MethodDefs and replace their usages (e.g. by overriding methods like transClassInternal)
    val transClasses = classes.flatMap {
      case classDef@ClassDef(annos, vis, name, genericTypeParams, parentClassRefs, content) if classDef.isGeneric => transClass(classDef)
      case classDef@ClassDef(annos, vis, name, genericTypeParams, parentClassRefs, content) => Seq(classDef)
    }
    Module(name, imports, transClasses)
  }

  override private[transformations] def transClassInternal(classDef: ClassDef): Seq[ClassDef] = {
    val ClassDef(annos, vis, name, typeParams, parents, content) = classDef
    val tyArgsSeq = polymorphicClassDefWithConcreteTypes.getOrElse(name,Seq()).distinct

    if (tyArgsSeq.isEmpty)
      return Seq(classDef)

    val classDefs = tyArgsSeq.map { // create a new classDef for every tyArgs used to create an instance of it
      tyArgs =>
        val monoName = polymorphicToMonomorphic(name, tyArgs)
        subst = mutable.Map(typeParams.map(_.name).zip(tyArgs):_*)

        val newContent = content.flatMap(c => transContent(c, classDef))

        // TODO necessary ?
//        val parentClasses = parents.map { c =>
//          val tname = TName(c.name)
//          tname.tyArgs = c.tyArgs
//          tname
//        }

        val newParents = parents.flatMap { tname =>
          val tyArgsSuper = polymorphicClassDefWithConcreteTypes.getOrElse(tname.name,Seq()).distinct
          //          val temp = tname.tyArgs.map {
          //            case TName(n) => subst(n)
          //          }
          val tnames: Seq[TName] = tyArgsSuper.map { tyArs =>
            tname.tyArgs = tyArs
            //TName(polymorphicToMonomorphic(tname.name,tyArs))
            ///TName(monomorphName(tname.name,tyArs))
            transType(tname) match {
              case tn@TName(_) => tn
              case _ => throw new Exception("expected TName")
            }
            //tname
          }.toSeq
          tnames
        }

        ClassDef(annos, vis, monoName, Seq(), newParents, newContent)
    }
    classDefs.toSeq
  }

  override private[transformations] def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): Seq[MethodDef] = {
    val MethodDef(annos, vis, name, genericTypeParams, params, outType, body) = methodDef

    val tyArgsSeqClass = polymorphicClassDefWithConcreteTypes(classDef.name).distinct

    val tyArgsSeq = polymorphicMethodDefWithConcreteTypes.getOrElse((classDef.name,name),Seq()).distinct

    val methodDefs = { // TODO refactor
      var monoName = name


      val methodDefsTemp = tyArgsSeq.map {
        tyArgs =>
          monoName = polymorphicToMonomorphic(name, tyArgs)
          subst ++= mutable.Map(genericTypeParams.map(_.name).zip(tyArgs): _*) // TODO test scoping ....

          val newParams = transParams(params)
          val newBody = transStatements(body)
          val newOutType = transType(outType)

          MethodDef(annos, vis, monoName, Seq(), newParams, newOutType, newBody)
      }

      val newParams = transParams(params)
      val newBody = transStatements(body)
      val newOutType = transType(outType)

      println("#### " + name)
      println(newOutType)

      methodDefsTemp :+ MethodDef(annos, vis, monoName, Seq(), newParams, newOutType, newBody)
    }
    methodDefs.toSeq.distinct
  }

  // TODO transExpressionInternal
  override private[transformations] def transExpressionInternal(expression: Expression): Seq[Expression] = Seq(expression match {
    case FieldReadExpr(recv, targetName) =>
      FieldReadExpr(transExpression(recv).head, targetName)
    case VarReadExpr(targetName) =>
      VarReadExpr(targetName)
    case constr@ConstructorExpr(n@TName(name), tyArgs, args) =>
      n.tyArgs = tyArgs
      val newName = transType(n) match {
        case newN@TName(_) => newN
        case _ => throw new Exception("expected TName in Constructor")
      }
      newName.tyArgs = Seq()
      val newConstr = ConstructorExpr(newName, Seq(), transExpressions(args))
      newConstr.tyParams = constr.tyParams.map(transType)
      newConstr
    case SuperExpr(args) =>
      SuperExpr(transExpressions(args))
    case MethodCallExpr(recv, funName, tyArgs, args, isFix) =>
      val newName = polymorphicToMonomorphic.getOrElse((funName,tyArgs),funName)
      MethodCallExpr(transExpression(recv).head, newName, Seq(), transExpressions(args), isFix)
    case TypeCastExpr(recv, toTyp) =>
      TypeCastExpr(transExpression(recv).head, transType(toTyp))
    case InstanceOfExpr(recv, ofTyp) =>
      InstanceOfExpr(transExpression(recv).head, transType(ofTyp))
    case TupleExpr(exps) =>
      TupleExpr(transExpressions(exps))
    case TupleReadExpr(recv, index) =>
      TupleReadExpr(transExpression(recv).head, index)
    case SetExpr(exps, tty) =>
      SetExpr(transExpressions(exps), if (tty.isDefined) Some(transType(tty.get)) else None)
    case SetMemberExpr(name, recv, predicate) =>
      val pred = if (predicate.isDefined) Some(transExpression(predicate.get).head) else None
      SetMemberExpr(name, transExpression(recv).head, pred)
    case SetFold(recv, projection, TName(name), method, neutral) =>
      SetFold(transExpression(recv).head, transExpressions(projection), TName(name), method, transExpression(neutral).head)
    case SetComprehension(exps, body) =>
      SetComprehension(transExpressions(exps), transExpression(body).head)
    case BaseApplyExpr(fun, args) =>
      BaseApplyExpr(fun, transExpressions(args))
    case BaseApplyInfixExpr(left, op, right) =>
      BaseApplyInfixExpr(transExpression(left).head, op, transExpression(right).head)
    case BaseApplyMethodExpr(recv, method, args) =>
      val argsOptions =
        if (args.isEmpty)
          None
        else
          Some(transExpressions(args.get))
      BaseApplyMethodExpr(transExpression(recv).head, method, argsOptions)
    case BaseApplyUnaryExpr(op, exp) =>
      BaseApplyUnaryExpr(op, transExpression(exp).head)
    case NullExpr() =>
      NullExpr()
    case BaseLitExpr(code) =>
      BaseLitExpr(code)
    case SetFromEdb(edbName, tty) =>
      SetFromEdb(edbName, transType(tty))
    case expr =>
      throw new RuntimeException(s"Can not transform expression: $expr")
  })

  // TODO finish transTypeInternal or fix subst (= map defined above)
  override private[transformations] def transTypeInternal(typ: Type): Type = {
    typ match {
      case TAny => TAny
      case TNull => TNull
      case TTuple(ts) => TTuple(ts.map(transType))
      case TSet(ty) => TSet(transType(ty))
      case TScala(ty) => TScala(ty)
      case TName(name) =>
        if (subst.contains(name)){
          return subst(name)
        }
        else if (polymorphicToMonomorphic.contains((name,typ.tyArgs))) {
          val newName = polymorphicToMonomorphic(name,typ.tyArgs)
          return TName(newName)
        }
        else {
          //val newName = polymorphicToMonomorphic.getOrElse(name,name)
          val ty = TName(name)
          ty.tyArgs = typ.tyArgs
          return ty
        }
      case tcls@TClass(TName(name)) =>
        if (subst.contains(name)) {
          return subst(name)
        }
        else if (polymorphicToMonomorphic.contains((name, typ.tyArgs))) {
          val newName = polymorphicToMonomorphic(name, typ.tyArgs)
          return TName(newName)
        }
        else {
          val ty = TClass(TName(name))
          ty.tyParams = tcls.tyParams.map(transType)
          return ty
        }
    }
  }


  private def generateMonomorphicVersionNames(): Unit = {
    polymorphicClassDef.foreach{
      case (name,classDef) =>
        polymorphicClassDefWithConcreteTypes.getOrElse(name, Seq()).foreach{tyArgs =>
          val monoName = monomorphName(name,tyArgs)
          polymorphicToMonomorphic(name -> tyArgs) = monoName
        }
    }
    polymorphicMethodDef.foreach{
      case ((clsName,name), methodDef) =>
        polymorphicMethodDefWithConcreteTypes.getOrElse((clsName,name),Seq()).foreach{tyArgs =>
          val monoName = monomorphName(name,tyArgs)
          polymorphicToMonomorphic(name -> tyArgs) = monoName
          //doesn`t matter if method with same name in other class since it will get same monoName if used with same tyArgs
        }
    }
  }

  private def monomorphName(name: Name, typeArgs: Seq[Type]): Name =
    Name(gensym.freshGlobal(name.toString + "$" + typeArgs.map(_.prettyprint).mkString))



  /*
  private def collectModuleContent(): Unit = {
    module.classes.foreach(classDef =>
      if (classDef.isGeneric) {polymorphicClassDef.put(classDef.name, classDef)}
      else {monomorphicClassDef.put(classDef.name, classDef)})

    module.classes.foreach(classDef => classDef.methods.foreach(methodDef =>
      if (methodDef.isGeneric) {polymorphicMethodDef.put(methodDef.name, methodDef)}
      else {monomorphicMethodDef.put(methodDef.name, methodDef)})) //TODO speichern der Methoden Klasse?? -> oben ergänzt

    println("polymophicClassDef :", polymorphicClassDef)
    println("monomophicClassDef :", monomorphicClassDef)
    println("polymophicMethodDef :", polymorphicMethodDef)
    println("monomophicMethodDef :", monomorphicMethodDef)
  }
   */

}
