package inca.frontend.objectoriented.lowering

import inca.frontend.objectoriented.core._
import inca.runtime.data.WrappedURI
import inca.util.Scala.typeOf
import truediff.GenericDiffable
import inca.runtime.aggregate.{Aggregation, AggregatorAssocComm}

import scala.meta.{Ctor, Name => MetaName, Type => MetaType, _}

class GenerateScala {
  class ScalaModule(val classes: Seq[Defn.Class], val objects: Seq[Defn.Object]) {
    lazy val objectMap: Map[String, Defn.Object] = objects.map(o => o.name.value -> o).toMap
    lazy val classMap: Map[String, Defn.Class] = classes.map(o => o.name.value -> o).toMap

    lazy val source: Source = {
      val stats: Seq[Stat] = classes ++ objects
      Source(stats.toList)
    }

    def syntax: String = {
      source.syntax
    }

    def main(mainObj: String, mainMethod: String): (Defn.Object, Defn.Def) = {
      val obj = objectMap.getOrElse(mainObj,
        throw new IllegalArgumentException(s"Could not find object with name ${mainObj}.")
      )
      val method = obj.templ.stats.find {
        case d: Defn.Def if d.name.value == mainMethod => true
        case _ => false
      }.getOrElse(
        throw new IllegalArgumentException(s"Could not find main method with name ${mainMethod}.")
      ).asInstanceOf[Defn.Def]
      (obj, method)
    }
  }

  val tGenericURI = typeOf[truechange.URI]
  val tGenericDiffable = typeOf[GenericDiffable]
  val tWrappedURI = typeOf[WrappedURI]

  def transType(t: Type): MetaType = t match {
    case TClass(ref) => MetaType.Name(ref.name.raw)
    case TAny =>  t.asScala
    case TTuple(ts) =>
      if (ts.nonEmpty)
        t"(..${ts.toList.map(transType)})"
      else
        MetaType.Name("Unit")
    case TScala(t) => t.tree
    case TSet(ty) => t"scala.collection.immutable.Set[${transType(ty)}]"
  }

  def genModule(module: Module): ScalaModule = {
    val (cls, objs) = module.classes.map(transClass).unzip
    new ScalaModule(cls, objs.flatten)
  }

  def transClass(classDef: ClassDef): (Defn.Class, Option[Defn.Object]) = {
    val cls = MetaType.Name(classDef.name.raw)
    val fields = classDef.fields.map(transField).toList

    val (defaultConstructorDef, constructorDef) = classDef.constructors.partition(_.params.isEmpty)
    val emptyDefaultConstructor = defaultConstructorDef.map(transEmptyDefaultConstructor).toList
    val constructors = constructorDef.flatMap(transConstructor).toList

    val (staticMethodDefs, methodDefs) = classDef.methods.partition(_.isStatic)
    val staticMethods = staticMethodDefs.flatMap(transMethod).toList
    val methods = methodDefs.flatMap(transMethod).toList

    val parentRefOption = classDef.parentClassRefs.headOption
    val parentTypeRef = if (parentRefOption.isDefined)
      Init(MetaType.Name(parentRefOption.get.name.raw) ,Term.Name(parentRefOption.get.name.raw), List())
    else
      Init(tGenericURI ,Term.Name(tGenericURI.toString()), List())
    //  Init(MetaType.Name("Object") ,Term.Name("Object"), List())
    // TODO: Use tGenericDiffable in the future ?
    // Init(tGenericDiffable ,Term.Name(tGenericDiffable.toString()), List())

    val clsDef = q"""class $cls() extends $parentTypeRef { this =>
      ..$fields
      ..$emptyDefaultConstructor
      ..$constructors
      ..$methods
    }"""

    val obj = Term.Name(classDef.name.raw)
    val objDefOption = {
      if (staticMethods.nonEmpty) {
        Some(
          q"""object $obj {
             ..$staticMethods
          }""")
      } else
        None
    }
    (clsDef, objDefOption)
  }

  def transField(fieldDef: FieldDef): Defn.Var = {
    val default = if (fieldDef.body.isDefined)
      Some(transExpression(fieldDef.body.get))
    else
      None
    // all fields are read / write now
    val vName = List(Pat.Var(Term.Name(fieldDef.name.raw)))
    Defn.Var(Nil, vName, Some(transType(fieldDef.typ)), default)
  }

  def transParam(param: Param): Term.Param = {
    Term.Param(Nil, Term.Name(param.name.raw), Some(transType(param.typ)), None)
  }

  def transEmptyDefaultConstructor(constructorDef: ConstructorDef): meta.Term = {
    val body = constructorDef.body.map(transStatement).toList
    q"(() => ${Term.Block(body)})()"
  }

  def transConstructor(constructorDef: ConstructorDef): Option[Ctor.Secondary] = {
    val params = constructorDef.params.map(transParam).toList
    val body = constructorDef.body.map(transStatement).toList
    val init = Init(MetaType.Singleton(Term.This(MetaName.Anonymous())), MetaName.Anonymous(), List(Nil))
    if (body.nonEmpty)
      Some(Ctor.Secondary(Nil, MetaName.Anonymous(), List(params), init, body))
    else
      None
  }

  def transMethod(methodDef: MethodDef): Option[Defn.Def] = {
    val methodName = Term.Name(methodDef.name.raw)
    val params = methodDef.params.map(transParam).toList
    val outTyp = transType(methodDef.outType)
    val body = methodDef.body.map(transStatement).toList
    val mods =
      if (methodDef.annos.contains(OverrideAnnotation))
        List(Mod.Override())
      else
        List()

    Some(
      q"""..${mods}def $methodName(..$params): $outTyp = {
        ..$body
      }"""
    )
  }

  def transStatement(stmt: Statement): meta.Stat = stmt match {
    case ExprStmt(expression) =>
      transExpression(expression)
    case ReturnStmt(expression) =>
      Term.Return(transExpression(expression))
    case FieldAssignStmt(recv, name, expression) =>
      val lhs = transExpression(recv)
      val rhs = transExpression(expression)
      val field = Term.Select(lhs, Term.Name(name.raw))
      Term.Assign(field, rhs)
    case VarDeclareStmt(name, typ, maybeExpression, immutable) =>
      val vTyp = Some(transType(typ))
      val value = if (maybeExpression.isDefined) Some(transExpression(maybeExpression.get)) else None
      val vName = List(Pat.Var(Term.Name(name.raw)))
      if (immutable)
        Defn.Val(Nil, vName, vTyp, value.getOrElse(Lit.Null()))
      else
        Defn.Var(Nil, vName, vTyp, value)
    case VarAssignStmt(targetName, expression) =>
      Term.Assign(Term.Name(targetName.raw), transExpression(expression))
    case VarPhiAssignStmt(name, typ, ifStmt, thnName, elsName) =>
      throw new RuntimeException("VarPhiAssignStmt is not supported!")
      /*val cond = transExpression(ifStmt.cnd)
      q"""
       val ${Pat.Var(Term.Name(name.raw))} = (if ($cond)
          vars(${thnName.raw})
       else
          vars(${elsName.raw})).asInstanceOf[${transType(typ)}]
       """*/
    case IfStmt(cnd, thn, els) =>
      val cond = transExpression(cnd)
      val thnStmts = Term.Block(thn.map(transStatement).toList)
      val elsStmts = Term.Block(els.map(transStatement).toList)
      Term.If(cond, thnStmts, elsStmts)
      /*val thnVars = thn.flatMap(_.vars).map(_._1.raw)
      val thnAssign = thnVars.map { v =>
        q"""vars = vars + (${Lit.String(v)} -> ${Term.Name(v)})"""
      }.toList
      val elsVars = els.flatMap(_.vars).map(_._1.raw)
      val elsAssign = elsVars.map { v =>
        q"""vars = vars + (${Lit.String(v)} -> ${Term.Name(v)})"""
      }.toList
      // TODO: Unpack the Block we return here
      q"""
       var vars: Map[String, Any] = Map()
       if ($cond) {
        ..${thn.map(transStatement).toList}
        ..$thnAssign
       } else {
         ..${els.map(transStatement).toList}
         ..$elsAssign
       }
       """*/
  }


  def transSetMemberExpression(expr: Expression): Seq[Enumerator] = expr match {
    case SetMemberExpr(name, recv, predicate) =>
      val rhs = transExpression(recv)
      val patVar = Pat.Var(Term.Name(name.raw))
      var res: Seq[Enumerator] = Seq(Enumerator.Generator(patVar, rhs))
      if (predicate.isDefined)
        res = res :+ Enumerator.Guard(transExpression(predicate.get))
      res
    case _ =>
      throw new IllegalArgumentException(s"Expression $expr is not a SetMemberExpression!")
  }

  def transExpression(expr: Expression): meta.Term = expr match {
    case VarReadExpr(targetName) => Term.Name(targetName.raw)
    case FieldReadExpr(recv, targetName) =>
      Term.Select(transExpression(recv), Term.Name(targetName.raw))
    case methodCallExpr@MethodCallExpr(recv, fun, args) =>
      val tArgs = args.map(transExpression).toList
      val tRecv = transExpression(recv)
      val tFun = Term.Select(tRecv, Term.Name(fun.raw))
      Term.Apply(tFun, tArgs)
    case TupleReadExpr(recv, index) =>
      val tRecv = transExpression(recv)
      Term.Select(tRecv, Term.Name("_"+index))
    case TupleExpr(exps) =>
      if (exps.nonEmpty)
        Term.Tuple(exps.map(transExpression).toList)
      else
        Lit.Unit()
    case BaseLitExpr(code) =>
      code.tree
    case BaseApplyExpr(fun, args) =>
      q"${fun.tree}(..${args.toList.map(e => transExpression(e))})"
    case BaseApplyInfixExpr(left, op, right) =>
      q"${transExpression(left)} ${op.tree} ${transExpression(right)}"
    case ConstructorExpr(classRef, args) =>
      val cArgs = args.map(transExpression).toList
      Term.New(Init(MetaType.Name(classRef.name.raw), MetaName.Anonymous(), List(cArgs)))
    case TypeCastExpr(recv, toTyp) =>
      // TODO: support tuples and sets
      val TClass(ClassRef(tyName)) = toTyp
      val term = Term.Select(transExpression(recv), Term.Name("asInstanceOf"))
      Term.ApplyType(term, List(MetaType.Name(tyName.raw)))
    case InstanceOfExpr(recv, ofTyp) =>
      // TODO: support tuples and sets
      ofTyp match {
        case TAny => ???
        case TNull =>
          q"${transExpression(recv)} == null"
        case TTuple(ts) => ???
        case TScala(ty) => ???
        case TClass(ClassRef(name)) =>
          val term = Term.Select(transExpression(recv), Term.Name("isInstanceOf"))
          Term.ApplyType(term, List(MetaType.Name(name.raw)))
        case TSet(ty) => ???
      }

    case BaseApplyMethodExpr(recv, method, args) =>
      val term = Term.Select(transExpression(recv), Term.Name(method.raw))
      val tArgs = args.getOrElse(Seq()).map(transExpression).toList
      Term.Apply(term, tArgs)
    case BaseApplyUnaryExpr(op, exp) =>
      Term.ApplyUnary(op.tree, transExpression(exp))
    case NullExpr() =>
      Lit.Null()
    case SetExpr(exps, _) =>
      val args = exps.map(transExpression).toList
      Term.Apply(Term.Name("Set"), args)
    case SetMemberExpr(name, recv, predicate) =>
      throw new IllegalArgumentException("Encountered unexpected SetMemberExpr!")
    case SetComprehension(member, body) =>
      Term.ForYield(member.flatMap(transSetMemberExpression).toList, transExpression(body))
    case superExpr@SuperExpr(args) =>
      val (_, superConstrDef) = superExpr.target.get
      val inParams = superConstrDef.params.map { p =>
        Term.Param(Nil, Term.Name(p.name.raw), Some(transType(p.typ)), None)
      }.toList
      val inTerms = args.map(transExpression).toList
      val superBody = superConstrDef.body.map(transStatement).toList
      q"((..$inParams) => (${Term.Block(superBody)}))(..$inTerms)"

    case SetFold(recv, opClass, opMethod, neutral) =>
      val foldTerm = Term.Select(transExpression(recv), Term.Name("fold"))
      val applyInner = Term.Apply(foldTerm, List(transExpression(neutral)))
      val methodRef = Term.Select(Term.Name(opClass.name.raw), Term.Name(opMethod.raw))
      Term.Apply(applyInner, List(methodRef))

    case _ =>
      throw new IllegalArgumentException(s"Expression '$expr' can not be translated to scala.")
  }

  def genAggregation(name: String, init: Expression, opClass: String, opMethod: String, typ: Type): meta.Term = {
    val scalaInit = transExpression(init)
    val scalaOp = q"${Term.Name(opClass)}.${Term.Name(opMethod)}"
    val scalaTy = transType(typ)

    val tyAggregation = typeOf[Aggregation[_]]
    val initAggregation = init"${MetaType.Apply(tyAggregation, List(scalaTy))}()"

    q"""
     new $initAggregation {
       override val name = $name
       override def init: $scalaTy = $scalaInit
       override def join(v1: $scalaTy, v2: $scalaTy): $scalaTy = $scalaOp(v1, v2)
       override val isAssociative = true
       override val isCommutative = true
     }"""
  }
}
