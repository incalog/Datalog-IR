package inca.frontend.objectoriented.lowering

import inca.frontend.objectoriented.core._
import inca.runtime.data.WrappedURI
import inca.util.Scala.typeOf
import truediff.GenericDiffable
import inca.runtime.aggregate.Aggregation
import inca.runtime.data.objectoriented.Identity

import scala.meta.{Ctor, Name => MetaName, Type => MetaType, _}

class GenerateScala {
  final case class BodyMustFailException(private val message: String = "") extends Exception(message, null)

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
    // TODO: Hack MonoMap
    val (cls, objs) = module.classes.map(transClass).unzip
    new ScalaModule(cls, objs)
  }

  def transClass(classDef: ClassDef): (Defn.Class, Defn.Object) = {
    val cls = MetaType.Name(classDef.name.raw)
    val fields = classDef.fields.map(transField).toList

    val (defaultConstructorDef, constructorDef) = classDef.constructors.partition(_.params.isEmpty)
    val emptyDefaultConstructor = defaultConstructorDef.map(transEmptyDefaultConstructor).toList
    val constructors = constructorDef.flatMap(transConstructor).toList
    val methods = classDef.methods.filter(!_.isStatic).flatMap(transMethod).toList

    val parentRefOption = classDef.parentClassRefs.headOption
    val parentTypeRef = if (parentRefOption.isDefined)
      Init(MetaType.Name(parentRefOption.get.name.raw) ,Term.Name(parentRefOption.get.name.raw), List())
    else
      Init(tGenericURI ,Term.Name(tGenericURI.toString()), List())
    //  Init(MetaType.Name("Object") ,Term.Name("Object"), List())
    // TODO: Use tGenericDiffable in the future
    // Init(tGenericDiffable ,Term.Name(tGenericDiffable.toString()), List())

    // We need to override equals and hash. Uncoalesing and coalesing are both input relations. That means they might
    // generate different scala objects when determining the input. Nevertheless these objects must be considered equal
    // if their attributes match. We can not use a case class, since we might inherit from a class, which is not
    // possible when using a case class. See: SetFoldSumObject for a test where these overrrides are required.

    var fieldComps = Seq(q"other.__identity == this.__identity")
    /*classDef.fields.map { f =>
      val fieldTerm = Term.Name(f.name.raw)
      q"""${Term.Name("other")}.$fieldTerm == ${Term.Name("this")}.$fieldTerm"""
    } :+ q"other.__identity == this.__identity" */
    fieldComps =
      if (parentRefOption.isDefined)
        q"super.equals(${Term.Name("other")}) == true" +: fieldComps
      else
        fieldComps

    val equalImpl =
      if (fieldComps.nonEmpty)
        fieldComps.reduce[Term.ApplyInfix] { case (c1, c2) => q"$c1 && $c2" }
      else
        q"true"

    var hashComps = q"${Term.Name("this")}.getClass.getSimpleName.##" +: classDef.fields.map { f =>
      val fieldTerm = Term.Name(f.name.raw)
      q"""${Term.Name("this")}.$fieldTerm.##"""
    } :+ q"this.__identity.##"
    hashComps = if (parentRefOption.isDefined) q"super.hashCode" +: hashComps else hashComps
    val hashCodeImpl = hashComps.reduce[Term] { case (c1, c2) => q"31 * ($c1) + $c2" }

    val clsBody = fields ++ emptyDefaultConstructor ++ constructors ++ methods
    val clsDef =
      if (parentRefOption.isDefined)
        q"""class $cls() extends $parentTypeRef {
          override def equals(that: Any): Boolean = that match {
            case other: $cls => $equalImpl
            case _ => false
          }
          override def hashCode(): Int = $hashCodeImpl

          ..$clsBody
        }"""
      else
        q"""class $cls() extends $parentTypeRef {
          var __identity: Option[inca.runtime.data.objectoriented.Identity] = None

          override def equals(that: Any): Boolean = that match {
            case other: $cls => $equalImpl
            case _ => false
          }
          override def hashCode(): Int = $hashCodeImpl

          ..$clsBody
        }"""

    val objDef = transCompanionObject(classDef)
    (clsDef, objDef)
  }

  private def transCompanionObject(classDef: ClassDef): Defn.Object = {
    val cls = MetaType.Name(classDef.name.raw)
    // TODO: We ignore set fields for now
    val fields = classDef.fields.filter(_.typ.asSet.isEmpty)
    val staticMethods = classDef.methods.filter(m => m.isStatic && !m.isMain).flatMap(transMethod).toList

    val identityObjectTerm = Term.Name("__identity")
    val identityObjectParam = Term.Param(Nil, identityObjectTerm, Some(typeOf[Identity]), None)
    // TODO: hack MonoMap
    val myFields = fields.filter { f =>
      !(f.typ.isInstanceOf[TClass] && f.typ.asInstanceOf[TClass].ref.name.raw.startsWith("MonoMap"))
    }
    val params = identityObjectParam +: myFields.flatMap { f =>
      f.typ.flatten.zipWithIndex.map { case (ty, i) =>
        Term.Param(Nil, Term.Name(f.name.raw + "$" + i), Some(transType(ty)), None)
      }
    }.toList

    def fieldToTuple(name: String, ty: Type, index: Int = 0): (Int, Term) = ty match {
      case TTuple(ts) =>
        var newIndex = index
        val terms = ts.map { ty =>
          val (idx, t) = fieldToTuple(name, ty, newIndex)
          newIndex = idx
          t
        }.toList
        (newIndex, q"(..$terms)")
      case _ =>
        (index + 1, q"${Term.Name(name + "$" + index)}")
    }

    val assignments = myFields.map { f =>
      val (newIndex, paramTerm) = fieldToTuple(f.name.raw, f.typ)
      val fieldTerm = Term.Name(f.name.raw)
      q"obj.$fieldTerm = $paramTerm"
    }.toList
    val newObj = Term.New(Init(cls, MetaName.Anonymous(), List(List())))

    val aggregationVal = if (classDef.isMonotoneClass) {
      val Some((_, resType)) = classDef.montoneTypes
      val scalaTy = transType(resType)
      val tyAggregation = typeOf[Aggregation[_]]
      val initAggregation = init"${MetaType.Apply(tyAggregation, List(scalaTy))}()"
      val monoType = Term.Name(classDef.name.raw)
      List(
        q"""
         lazy val __aggregation__ = {
           new $initAggregation {
             override val name = ${classDef.name.raw}
             override def init: $scalaTy = $monoType.init()
             override def join(v1: $scalaTy, v2: $scalaTy): $scalaTy = $monoType.join(v1, v2)
             override val isAssociative = true
             override val isCommutative = true
           }
         }"""
      )
    } else {
      Nil
    }

    val obj = Term.Name(classDef.name.raw)

    // the apply method is used for coalesing and uncoalesing
    q"""object $obj {
        def apply(..$params): $cls = {
          val obj = $newObj
          obj.__identity = Some($identityObjectTerm)
          ..$assignments
          obj
        }
        ..$aggregationVal
        ..$staticMethods
    }"""
  }

  def transField(fieldDef: FieldDef): Defn.Var = {
    val default = if (fieldDef.body.isDefined)
      Some(transExpression(fieldDef.body.get))
    else
      None
    // all fields are read / write
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
    val body = try {
      methodDef.body.map(transStatement).toList
    } catch {
      case BodyMustFailException(message) =>
        val runtimeException = q"""throw new RuntimeException($message)"""
        List(runtimeException)
    }

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
      val field = Term.Select(lhs, Term.Name(name.raw))
      val rhs = transExpression(expression)
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
      val transRecv = transExpression(recv)
      val fieldTerm = Term.Name(targetName.raw)
      val default = Term.Select(transRecv, fieldTerm)
      recv.typ match {
        case Some(TClass(ref)) =>
          ref.target match {
            case Some(classDef) if classDef.isMonotoneClass && targetName.raw == "result" =>
              throw BodyMustFailException("Illegal usage of result field!")
            case None | Some(_) => default
          }
        case Some(_) => default
        case None => throw new IllegalArgumentException(s"Untyped expression $expr!")
      }

    case MethodCallExpr(recv, fun, args, _) =>
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
      if (args.isDefined) {
        val tArgs = args.get.map(transExpression).toList
        Term.Apply(term, tArgs)
      } else {
        term
      }
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
    case setFold@SetFold(recv, filter, opClass, opMethod, neutral) =>
      // TODO: How to use the filter correctly
      q"???"
      /*val aggIndex = setFold.aggIndex
      val aggType = setFold.typ.get.flatten(aggIndex).asScala
      val recvTerm = transExpression(recv)
      val neutralTerm = transExpression(neutral)
      val methodRef = Term.Select(Term.Name(opClass.name.raw), Term.Name(opMethod.raw))
      q"""$recvTerm.toList.asInstanceOf[Seq[Any]].map {
          case p: Product => p.$aggIndex
          case e => e
      }.asInstanceOf[Seq[$aggType]].fold($neutralTerm)($methodRef)
      """*/

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
