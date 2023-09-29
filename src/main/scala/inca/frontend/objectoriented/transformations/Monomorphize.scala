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
        case SuperExpr(args) =>
          args.foreach(collectExpression(_))
        case MethodCallExpr(recv, fun, tyArgs, args, isFix) =>
          collectExpression(recv)
          if (tyArgs.nonEmpty){
            if (polymorphicMethodDefWithConcreteTypes.keys.exists(_ == (classDef.name,fun))) {
              polymorphicMethodDefWithConcreteTypes(classDef.name,fun).append(tyArgs)
            }
            else {
              polymorphicMethodDefWithConcreteTypes += ((classDef.name,fun) -> mutable.ArrayBuffer(tyArgs))
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

  override def transMethodInternal(methodDef: MethodDef, classDef: ClassDef): Seq[MethodDef] = {
    val MethodDef(annos, vis, name, genericTypeParams, params, outType, body) = methodDef

    val newParams = transParams(params)
    val newBody = transStatements(body)
    val newOutTypes = transType(outType)
    Seq(methodDef)
  }

  override def transClassInternal(classDef: ClassDef): Seq[ClassDef] = {
    val ClassDef(annos, vis, name, genericTypeParams, parentClassRefs, content) = classDef
    Seq(classDef)
  }


  override def transExpressionInternal(expression: Expression): Seq[Expression] = super.transExpressionInternal(expression)


  */
  override private[transformations] def transModuleInternal(module: Module): Module = {
    val Module(name, imports, classes) = module
    // make sure the class name is unique
    gensym.register(module.usedModuleNames.map(_.raw))
    gensym.register(classes.map(_.name.raw))
    val transClasses = classes.flatMap(transClass)
    collectTypeApplications()
    Module(name, imports, transClasses)
  }



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
