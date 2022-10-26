package inca.frontend.objectoriented.lowering

import inca.compiler.SourceLocation
import inca.runtime.aggregate.{Aggregation, AggregatorAssocComm}
import inca.frontend.objectoriented.core._
import inca.runtime.data.WrappedURI
import inca.util.Scala.{symbolOf, typeOf}
import truediff.GenericDiffable

import scala.meta.{Name => MetaName, Type => MetaType, _}

class GenerateScala {
  class ScalaModule(val classes: Seq[Defn.Class], val objects: Seq[Defn.Object]) {
    lazy val objectMap: Map[String, Defn.Object] = objects.map(o => o.name.value -> o).toMap
    lazy val classMap: Map[String, Defn.Class] = classes.map(o => o.name.value -> o).toMap

    lazy val source: Source = {
      val stats: Seq[Stat] = classes ++ objects
      Source(stats.toList)
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

  val tGenericDiffable = typeOf[GenericDiffable]
  val tWrappedURI = typeOf[WrappedURI]

  def transType(t: Type): MetaType = t match {
    case TClass(ref) => MetaType.Name(ref.name.raw)
    case TAny =>  t.asScala
    case TTuple(ts) => t"(..${ts.toList.map(transType)})"
    case TScala(t) => t.tree
    case TSet(ty) => t"scala.Set[${transType(ty)}]"
  }

  def genModule(module: Module): ScalaModule = {
    val (cls, objs) = module.classes.map(genClass).unzip
    new ScalaModule(cls, objs.flatten)
  }

  def genClass(classDef: ClassDef): (Defn.Class, Option[Defn.Object]) = {
    val cls = MetaType.Name(classDef.name.raw)
    val fields = classDef.fields.flatMap(transField).toList
    val constructors = classDef.constructors.map(transConstructor).toList
    val methods = classDef.methods.filter(!_.isMain).flatMap(transMethod).toList
    val mainMethods = classDef.methods.filter(_.isMain).flatMap(transMethod).toList
    // TODO: Support multiple inheritance in the future
    val parentRefOption = classDef.parentClassRefs.headOption
    val parentTypeRef = if (parentRefOption.isDefined)
      Init(MetaType.Name(parentRefOption.get.name.raw) ,Term.Name(parentRefOption.get.name.raw), List())
    else
      Init(MetaType.Name("Object") ,Term.Name("Object"), List())
    // TODO: Import this in the future
    /*else
      Init(tGenericDiffable ,Term.Name(tGenericDiffable.toString()), List())*/
    val clsDef = q"""class $cls(..$fields) extends $parentTypeRef { this =>
      ..${constructors.flatten}
      ..$methods
    }"""

    val obj = Term.Name(classDef.name.raw)
    val objDefOption = {
      if (mainMethods.nonEmpty) {
        Some(q"""object $obj { ..$mainMethods }""")
      } else
        None
    }
    (clsDef, objDefOption)
  }

  def transField(fieldDef: FieldDef): Option[Term.Param] = {
    try {
      val default = if (fieldDef.body.isDefined)
        Some(transExpression(fieldDef.body.get))
      else
        None
      // all fields are read / write now
      val mods = List(Mod.VarParam())
      Some(Term.Param(mods, Term.Name(fieldDef.name.raw), Some(fieldDef.typ.asScala), default))
    } catch {
      case e: Throwable =>
        println("Error: ", e)
        None
    }
  }

  def transParam(param: Param): Term.Param = {
    Term.Param(Nil, Term.Name(param.name.raw), Some(transType(param.typ)), None)
  }

  lazy val emptySuperCall: Init =
    Init(MetaType.Singleton(Term.This(MetaName.Anonymous())), MetaName.Anonymous(), List(Nil))

  def transConstructor(constructorDef: ConstructorDef): Option[Ctor.Secondary] = {
    val params = constructorDef.params.map(transParam).toList
    val bodyStmts = constructorDef.body.map(transStatement).toList
    val init = bodyStmts.find(_.isInstanceOf[Init]).getOrElse(emptySuperCall).asInstanceOf[Init]
    val body = bodyStmts.filter(!_.isInstanceOf[Init])
    if (body.nonEmpty)
      Some(q"""def this(..$params) = {
        $init
        ..$body
      }""")
    else
      None
  }

  def transMethod(methodDef: MethodDef): Option[Defn.Def] = {
    val methodName = Term.Name(methodDef.name.raw)
    val params = methodDef.params.map(transParam).toList
    val outTyp = transType(methodDef.outType)
    try {
      val body = Term.Block(methodDef.body.map(transStatement).toList)
      Some(
        q"""def $methodName(..$params): $outTyp = {
          $body
        }"""
      )
    } catch {
      case e: Throwable =>
        println("Error: ", e)
        None
    }

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
      val value = if (maybeExpression.isDefined) transExpression(maybeExpression.get) else Lit.Null()
      val vName = List(Pat.Var(Term.Name(name.raw)))
      if (immutable)
        Defn.Val(Nil, vName, vTyp, value)
      else
        Defn.Var(Nil, vName, vTyp, Some(value))
    case VarAssignStmt(targetName, expression) =>
      Term.Assign(Term.Name(targetName.raw), transExpression(expression))
    case VarPhiAssignStmt(name, typ, ifStmt, thnName, elsName) =>
      // TODO: this won't work. Instead: Accumulate all vars in a map in the if stmt and access them here
      //  or enforce that we translate a none optimized module
      val cond = transExpression(ifStmt.cnd)
      val vName = List(Pat.Var(Term.Name(name.raw)))
      val ifTerm = Term.If(cond, Term.Name(thnName.raw), Term.Name(elsName.raw))
      Defn.Val(Nil, vName, Some(transType(typ)), ifTerm)
    case IfStmt(cnd, thn, els) =>
      val cond = transExpression(cnd)
      val thnStmts = Term.Block(thn.map(transStatement).toList)
      val elsStmts = Term.Block(els.map(transStatement).toList)
      Term.If(cond, thnStmts, elsStmts)
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
    case MethodCallExpr(recv, fun, args) =>
      val tArgs = args.map(transExpression).toList
      val tRecv = transExpression(recv)
      val tFun = Term.Select(tRecv, Term.Name(fun.raw))
      Term.Apply(tFun, tArgs)
    case TupleReadExpr(recv, index) =>
      val tRecv = transExpression(recv)
      Term.Select(tRecv, Term.Name("_"+index))
    case TupleExpr(exps) => ???
      val tExps = exps.map(transExpression).toList
      Term.Tuple(tExps)
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
      val TClass(ClassRef(tyName)) = ofTyp
      val term = Term.Select(transExpression(recv), Term.Name("isInstanceOf"))
      Term.ApplyType(term, List(MetaType.Name(tyName.raw)))
    case BaseApplyMethodExpr(recv, method, args) =>
      val term = Term.Select(transExpression(recv), Term.Name(method.raw))
      val tArgs = args.getOrElse(Seq()).map(transExpression).toList
      Term.Apply(term, tArgs)
    case BaseApplyUnaryExpr(op, exp) =>
      Term.ApplyUnary(op.tree, transExpression(exp))
    case NullExpr() =>
      Lit.Null()
    case SetExpr(exps) =>
      val args = exps.map(transExpression).toList
      Term.Apply(Term.Name("Set"), args)
    case SetMemberExpr(name, recv, predicate) =>
      throw new IllegalArgumentException("Encountered unexpected SetMemberExpr!")
    case SetComprehension(member, body) =>
      Term.ForYield(member.flatMap(transSetMemberExpression).toList, transExpression(body))

    /*case SetReduce(recv, op) => ???*/

    case _ =>
      throw new IllegalArgumentException(s"Expression '$expr' can not be translated to scala.")
  }
}
