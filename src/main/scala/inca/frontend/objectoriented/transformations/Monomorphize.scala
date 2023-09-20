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
  private val polymorphicMethodDef = mutable.Map[Name, MethodDef]()

  private val polymorphicCallSites = mutable.Map[Name, Seq[(Name, Seq[(Name, Seq[Type])])]]()
  // className -> Seq[MethodName, Seq[ParamName, Seq[Types]]]]


  private val polymorphicToMonomorphic = mutable.Map[(Name, Seq[Type]), Name]()


    private def collectTypeApplications(): Seq[(Name, Seq[Type])] = {
      module.classes.foreach(clsDef => collectClassDef(clsDef))

      def collectClassDef(classDef: ClassDef): Seq[(Name, Seq[(Name, Seq[Type])])] = {
        //Constructor filtern!
        classDef.methods.foreach(methodDef => collectMethodDef(classDef, methodDef))
        /*
        val gpTypes: Seq[Type] = ??? // gesammelte Typen
        val genTypes = classDef.genericTypeParams.map(gp => (gp.name, gpTypes))

        Seq((classDef.name,genTypes))

         */
        Seq()
      }


      def collectMethodDef(classDef: ClassDef, methodDef: MethodDef): Seq[(Name, Seq[Type])] = {
        val tys = (methodDef.params.map(_.typ) ++
          Seq(methodDef.outType) ++
          methodDef.genericTypeParams.map(gp => TName(gp.name))).distinct //distinct??



        methodDef.body.foreach(stm => stm match {
          case VarDeclareStmt(name, typ, maybeExpression, immutable) => maybeExpression match {
            case Some(value) => value match {
              case ConstructorExpr(classRef, tyArgs, args) => println("ClassName: ", classRef.name, " GenericParamArgs: ", tyArgs) //Initialisierung mit TyArgs Typ
              case MethodCallExpr(recv, fun, tyArgs, args, isFix) => println("FromClass: ", recv.vars.get(recv match {
                case VarReadExpr(targetName) => targetName }) match {
                  case Some(value) => value match {
                    case Some(value) => value match {
                      case TClass(ref) => ref.name
                    }
                  }
                }
              , " methodName: ", fun, " GenericParamArgs: ", tyArgs)
            }
          } //Seq(name, typ)
          case _ => print("\n")
        })




        Seq()
      }
      Seq()
    }

    // Output Seq[(ClassName, Seq[GenParamName, Seq[Type]])]

      /*
      def collectExpression(exp: Expression): Seq[(Name, Seq[Type])] = ???



      def collectStatement(stm: Statement): Seq[(Name, Seq[Type])] = {
        stm match {
          case ExprStmt(expression) => collectExpression(expression)
          case ReturnStmt(expression) => collectExpression(expression)
          //case FieldAssignStmt(recv, name, expression) => collectExpression(expression)
          //case VarDeclareStmt(name, typ, maybeExpression, immutable) => collectExpression(expression)
          //case VarAssignStmt(targetName, expression) => collectExpression(expression)
          //case VarPhiAssignStmt(name, typ, ifStmt, thnName, elsName) => collectExpression(expression)
          //case IfStmt(cnd, thn, els) => collectExpression(expression)
         }
       }





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



  private def collectModuleContent(): Unit = {
    module.classes.foreach(classDef =>
      if (classDef.isGeneric) {polymorphicClassDef.put(classDef.name, classDef)}
      else {monomorphicClassDef.put(classDef.name, classDef)})

    module.classes.foreach(classDef => classDef.methods.foreach(methodDef =>
      if (methodDef.isGeneric) {polymorphicMethodDef.put(methodDef.name, methodDef)}
      else {monomorphicMethodDef.put(methodDef.name, methodDef)})) //TODO speichern der Methoden Klasse??

    println("polymophicClassDef :", polymorphicClassDef)
    println("monomophicClassDef :", monomorphicClassDef)
    println("polymophicMethodDef :", polymorphicMethodDef)
    println("monomophicMethodDef :", monomorphicMethodDef)
  }

}
