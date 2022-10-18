package inca.frontend.objectoriented.lowering

import inca.compiler.SourceLocation
import inca.runtime.aggregate.{Aggregation, AggregatorAssocComm}
import inca.frontend.objectoriented.core._
import inca.runtime.data.WrappedURI
import inca.util.Scala.{symbolOf, typeOf}
import truediff.GenericDiffable

import scala.meta.{Type => MetaType, _}

class GenerateScala {
  val tGenericDiffable = typeOf[GenericDiffable]
  val tWrappedURI = typeOf[WrappedURI]


  private var visited: Map[Any, meta.Stat] = Map()
  private def createIfNeeded(a: Any)(f: => meta.Stat): Unit = visited.get(a) match {
    case None =>
      //this.visited += a -> Seq()
      //val stats = f
      this.visited += a -> f
    case Some(_) => // nothing
  }

  def generated: List[meta.Stat] = visited.values.toList

  def transType(t: Type): MetaType = t match {
    case TClass(ref) => MetaType.Name(ref.name.raw)
    case TAny =>  t.asScala
    case TTuple(ts) => t"(..${ts.toList.map(transType)})"
    case TScala(t) => t.tree
    case TSet(ty) => t"scala.Set[${transType(ty)}]"
  }

  def genModule(module: Module): Unit = {
    module.classes.foreach(genClass)
  }

  def genClass(classDef: ClassDef): Unit = createIfNeeded(classDef) {
    val cls = MetaType.Name(classDef.name.raw)
    val fields = classDef.fields.flatMap(transField).toList
    //val constructor = classDef.constructors.map(transConstructor).toList
    val methods = classDef.methods.flatMap(transMethod).toList
    // TODO: extend with possible superclass
    q"""class $cls(..$fields) extends {} with $tGenericDiffable() { this =>
      ..$methods
    }"""
  }

  def transField(fieldDef: FieldDef): Option[Term.Param] = {
    try {
      val default = if (fieldDef.body.isDefined)
        Some(transExpression(fieldDef.body.get))
      else
        None
      // all fields are readonly for now !
      val mods = List(Mod.ValParam())
      Some(Term.Param(mods, Term.Name(fieldDef.name.raw), Some(fieldDef.typ.asScala), default))
    } catch {
      case _: Throwable => None
    }
  }

  def transParam(param: Param): Term.Param = {
    Term.Param(Nil, Term.Name(param.name.raw), Some(transType(param.typ)), None)
  }

  /*def transConstructor(constructorDef: ConstructorDef): Ctor.Secondary = {
    val params = constructorDef.params.map(transParam).toList
    val body = Term.Block(constructorDef.body.map(transStatement).toList)
    q"""def this(..$params) = {
      $body
    }"""
  }*/

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
      case _: Throwable => None
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

    //case BaseApplyMethodExpr(recv, method, args) => ???
    //case BaseApplyUnaryExpr(op, exp) => ???
    // case NullExpr() =>
    //case TypeCastExpr(recv, toTyp) => ???
    //case InstanceOfExpr(recv, ofTyp) => ???

    /*case ConstructorExpr(classRef, args) =>
      val cArgs = args.map(transExpression).toList
      q"new ${classRef.name.raw}(..$cArgs)"

    case SetExpr(exps) => ???
    case SetMemberExpr(name, recv, predicate) => ???
    case SetComprehension(member, body) => ???
    case SetReduce(recv, op) => ???*/

    case _ =>
      throw new IllegalArgumentException(s"Expression '$expr' can not be translatet to scala.")
  }


  def genAggregation(op: MethodDef): meta.Term = {
    val typ = op.outType
    val name = op.name.raw

    val scalaTy = transType(typ)

    q"""meta.Term
     new inca.runtime.aggregate.Aggregation {
       override val name = $name
       override def init: $scalaTy = null
       override def join(v1: $scalaTy, v2: $scalaTy): $scalaTy = if (v1 != null) v1.${Term.Name(op.name.raw)}(v2) else v2
       override val isAssociative = true
       override val isCommutative = true
     }"""
  }
}
