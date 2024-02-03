package inca.frontend.constraint.typechecker

import inca.compiler.SourceLocation
import inca.frontend.constraint.core._
import inca.frontend.util.{Resolvable, Typeable}
import inca.runtime.aggregate.Aggregation
import inca.runtime.context.DataModel
import inca.util.Scala
import truechange.{AnyType, ListType, SortType}

trait CoreTypechecker
  extends TypeContext with TypeIO with ScalaTypeContext {

  val dataModel: DataModel

  def typecheck(program: Seq[Module]): Unit = scopedTypeContext {
    program.foreach(bindModule)
    program.foreach(typecheck)
  }

  /*
   * Module
   */

  def typecheck(module: Module): Unit = scopedTypeContext {
    for (imp <- module.imports;
         importedModule <- lookupModule(imp.name)) {
      resolveTarget(imp)(importedModule)

      for (content <- importedModule.content if !content.vis.contains(Private)) {
        content match {
          case fun: PatternFunction => bindFun(fun, importedModule)
          case valDef: ValDef => bindVar(valDef.name, valDef, valDef.getType.get)
          case _: ScalaModuleContent[_] => // nothing
        }
      }
    }

    // add types of language meta info to typing context
    dataModel.types.foreach { typ =>
      bindNode(TNode(typ.name), TNode(typ.name))
    }

    // import nodes
    module.nodeImports.foreach { nodeimp =>
      validNodeImport(nodeimp)
    }


    // bind symbols first
    module.content.foreach {
      case fun: PatternFunction => bindFun(fun, module)
      case _: ValDef => // scoped to remainder of module, hence bind later
      case ScalaModuleContent(Scala(imp: meta.Import)) => registerImport(imp)
      case ScalaModuleContent(Scala(stat: meta.Stat)) => registerBlockDef(stat)
    }

    // type scala top-level definitions
    typecheckTopLevelObject()

    module.content.foreach {
      case fun: PatternFunction => typecheck(fun)
      case vd: ValDef => typecheck(vd)
      case _: ScalaModuleContent[_] => // nothing
    }
  }

  def validNodeImport(nodeImport: NodeImport): Unit = {
    val path = nodeImport.name.name.split('.')
    if (path.nonEmpty) {
      if (path.last == "_") {
        val prefix = path.init.mkString(".")
        val typesToImport = prefixedNodes(prefix)
        if (typesToImport.isEmpty) {
          warn(s"The wildcard import node ${nodeImport.name} resolves to an empty set of imported nodes", nodeImport)
        }
        importPrefixed(prefix)
      } else {
        val nodes = prefixedNodes(nodeImport.name.name)
        if (nodes.isEmpty)
          warn(s"The imported node ${nodeImport.name} cannot be found", nodeImport)
        importPrefixed(nodeImport.name.name)
      }
    } else
      warn(s"Path of imported node ${nodeImport} is empty", nodeImport)
  }

  private def prefixedNodes(prefix: String): Set[SortType] = dataModel.types.filter { typ =>
    typ.name.startsWith(prefix)
  }

  private def importPrefixed(prefix: String): Unit = prefixedNodes(prefix).foreach { typ =>
    bindNode(TNode(typ.name.substring(prefix.length+1)), TNode(typ.name))
  }



  /*
   * Function
   */
  def typecheck(fun: PatternFunction): Unit = scopedTypeContext {
    val inputScalaParams = fun.params.collect{ case p@Param(_, TScala(_)) => p }
    inputScalaParams.foreach { param =>
      error(s"Pattern functions do not allow input parameter of Scala type ${param.typ}", fun)
    }
    fun.params.foreach { p =>
      validType(p.typ)
      bindVar(p.name, p, p.typ)
    }
    validType(fun.outType)
    fun.bodies.foreach { body =>
      val ty = typecheck(body, mustYield = true)
      if (!subtype(ty.asType, fun.outType, dataModel))
        error(s"Found body of type $ty, but expected function result type ${fun.outType}", body)
    }
  }

  def validType(typ: Type): Unit = typ match {
    case node: TNode => lookupNode(node) match {
      case Some(value) =>
        val x = value
        resolveTarget(node)(value)
      case None => // nothing
    }
    case iterable: TIterable => validType(iterable.contained)
    case TTuple(ts) => ts.foreach(validType)
    case TScala(_) => // nothing
    case TAny => // nothing
    case TNothing => // nothing
    case TLiteral(litType) => // nothing
  }

  def typecheck(body: Body, mustYield: Boolean): StmType = scopedTypeContext {
    if (body.stmts.isEmpty) {
      if (mustYield) {
        warn(s"Block should end with a terminator statement", body)
        Yields(TUnit)
      } else {
        NoYield
      }
    }
    else {
      body.stmts.init.foreach { stm =>
        typecheck(stm, mustYield = false, mayYield = false)
      }
      typecheck(body.stmts.last, mustYield, mayYield = true)
    }
  }

  /*
   * ValDef
   */

  def typecheck(valDef: ValDef): Unit = {
    val inferred = typecheck(valDef.exp)
    valDef.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated, dataModel))
          error(s"Inferred type $inferred, but expected annotated type $annotated", valDef)
        bindVar(valDef.name, valDef, annotated)
      case None =>
        bindVar(valDef.name, valDef, inferred)
    }
  }

  /*
   * Statements
   */

  final def typecheck(stm: Statement, mustYield: Boolean, mayYield: Boolean): StmType = typecheckInternal(stm, mustYield) match {
    case ty: NoYield.type =>
      if (mustYield) {
        warn(s"Block should end with a terminator statement", stm)
        Yields(TUnit)
      } else {
        ty
      }
    case ty: Yields =>
      if (!mayYield) {
        warn(s"Unexpected terminator statement", stm)
        NoYield
      } else  {
        ty
      }
  }

  protected def typecheckInternal(stm: Statement, mustYield: Boolean): StmType = stm match {
    case core: CoreStatement => typecheckCore(core)
    case _ => throw new UnsupportedOperationException(s"No type rule for $stm found.")
  }

  final def typecheckCore(stm: CoreStatement): StmType = stm match {
    case FailStatement =>
      Yields(TNothing)

    case Yield(exp) =>
      val ty = typecheck(exp)
      Yields(ty)

    case Assert(cond) =>
      val condTy = typecheck(cond)
      if (!subtype(condTy, TScalaBoolean, dataModel))
        error(s"Expected Boolean condition, but got $condTy", cond)
      NoYield

    case vals@Values(name, typ) =>
      validType(typ)
      typ match {
        case TNothing => warn(s"$typ contains no values, enumeration will fail", typ)
        case TScala(_) => error(s"Cannot enumerate Scala values of type $typ", typ)
        case _ => // fine
      }
      bindVar(name, vals, typ)
      NoYield

    case as@Assign(names, exp) =>
      val ty = typecheck(exp)
      validType(ty)

      val namesStr = names.mkString("(", ", ", ")")
      ty match {
        case TUnit =>
          if (names.nonEmpty)
            error(s"Cannot assign expression of type $TUnit to $namesStr", stm)
          names.foreach(bindVar(_, as, TAny))
        case TTuple(tys) =>
          if (names.size != tys.size)
            error(s"Cannot assign ${tys.size}-ary tuple to $namesStr", stm)
          names.zipAll(tys, null, null).foreach {
            case (name, null) => bindVar(name, as, TAny)
            case (null, ty) => // nothing
            case (name, ty) => bindVar(name, as, ty.unroll)
          }
        case ty =>
          if (names.size != 1)
            error(s"Cannot assign expression of type $ty to $namesStr", stm)
          names.zipAll(Seq(ty), null, null).foreach {
            case (name, null) => bindVar(name, as, TAny)
            case (null, ty) => // nothing
            case (name, ty) => bindVar(name, as, ty.unroll)
          }
      }
      NoYield
  }


  /*
   * Expressions
   */

  final def typecheck(exp: Expression): Type = assignType(exp)(typecheckInternal(exp, exp.typ))

  protected def typecheckInternal(exp: Expression, anno: Option[Type]): Type = exp match {
    case core: CoreExpression => typecheckCore(core, anno)
    case _ => throw new UnsupportedOperationException(s"No type rule for $exp found.")
  }

  final def typecheckCore(exp: CoreExpression, anno: Option[Type]): Type = exp match {
    case v@Var(name) =>
      lookupVar(name) match {
        case Some((decl, ty)) =>
          resolveTarget(v)(decl)
          ty
        case None =>
          TAny
      }

    case Eq(lhs, rhs) =>
      val lty = typecheck(lhs)
      val rty = typecheck(rhs)
      if (meet(lty, rty, dataModel) == TNothing) {
        error(s"Cannot compare left-hand $lty with right-hand $rty", exp)
      }
      TScalaBoolean

    case Neq(lhs, rhs) =>
      val lty = typecheck(lhs)
      val rty = typecheck(rhs)
      if (meet(lty, rty, dataModel) == TNothing) {
        error(s"Cannot compare left-hand $lty with right-hand $rty", exp)
      }
      TScalaBoolean

    case InstanceOf(e, ty) =>
      val ety = typecheck(e)
      validType(ty)
      if (meet(ety, ty, dataModel) == TNothing) {
        warn(s"Test of type $ety to unrelated type $ty will always fail", exp)
      }
      TScalaBoolean

    case NotInstanceOf(e, ty) =>
      val ety = typecheck(e)
      validType(ty)
      if (meet(ety, ty, dataModel) == TNothing) {
        warn(s"Test of type $ety to unrelated type $ty will always succeed", exp)
      }
      TScalaBoolean

    case Cast(src, targetTyp) =>
      val ety = typecheck(src)
      validType(targetTyp)
      if (meet(ety, targetTyp, dataModel) == TNothing) {
        warn(s"Cast of type $ety to unrelated type $targetTyp will always fail", exp)
      }
      targetTyp

    case Def(e) =>
      typecheck(e)
      if (!isValidDefUndefExp(e)) {
        error(s"Cannot test definedness of ${e.getClass.getName} expression", exp)
      }
      TScalaBoolean

    case Undef(e) =>
      typecheck(e)
      if (!isValidDefUndefExp(e)) {
        error(s"Cannot test definedness of ${e.getClass.getName} expression", exp)
      }
      TScalaBoolean

    case Wildcard =>
      TAny

    case Constant(lit) =>
      typecheckLiteral(lit)

    case PathAccess(receiver, link) =>
      val rty = typecheck(receiver)
      typecheckLink(link, rty, exp)

    case call@Call(name, args, transitive) =>
      lookupFun(name) match {
        case None =>
          args.foreach(typecheck)
          TAny
        case Some(fun) =>
          resolveTarget(call)(fun)
          typecheckCall(fun, args, transitive, exp)
      }

    case Count(call) =>
      typecheck(call)
      TScalaInt

    case Tuple(exps) =>
      exps.map(typecheck) match {
        case Seq() => TUnit
        case Seq(ty) => ty
        case tys => TTuple(tys)
      }

    case eval@Eval(code) =>
      typecheckEval(code, eval)

    case Aggregate(agg, bodies) =>
      val aggTy = typecheck(agg)

      val bodiesTy = bodies.foldLeft[Type](TAny) { (bodiesTy, body) =>
        val Yields(ty) = typecheck(body, mustYield = true)
        meet(bodiesTy, ty, dataModel)
      }

      val bodiesScalaTy = bodiesTy.asScala
      val requiredAggTy = TScala(Scala(meta.Type.Apply(Scala.typeOf[Aggregation[_]], List(bodiesScalaTy))))

      if (!subtype(aggTy, requiredAggTy, dataModel))
        error(s"Expected $requiredAggTy, but got $aggTy", agg)

      TScala(Scala(bodiesScalaTy))
  }


  def isValidDefUndefExp(exp: Expression): Boolean = exp match {
    case _: PathAccess => true
    case _: Call => true
    case _ => false
  }

  def typecheckLiteral(lit: Literal): Type = lit match {
    case UnitLiteral => TUnit
    case BooleanLiteral(_) => TScalaBoolean
    case IntLiteral(_) => TScalaInt
    case LongLiteral(_) => TScalaLong
    case DoubleLiteral(_) => TScalaDouble
    case StringLiteral(_) => TScalaString
  }

  final def typecheckLink(link: Link, receiverTy: Type, exp: Expression): Type = link match {
    case NamedLink(field: Name) => receiverTy match {
      case node: TNode =>
        val nodeName = node.target.getOrElse(TNode("")).name
        dataModel.links.get(nodeName, field.name) match {
          case Some(tcty) =>
            val ty = truechangeTypeToType(tcty)
            validType(ty)
            ty
          case _ => dataModel.litLinks.get(nodeName, field.name) match {
            case Some(litType) => TLiteral(litType)
            case _ =>
              error(s"Cannot access field `$field` of node $node", exp)
              TAny
          }
        }
      case _ =>
        error(s"Cannot access field `$field` of non-node type $receiverTy", exp)
        TAny
    }
    case ParentLink => TAny
    case ChildrenLink =>
      receiverTy match {
        case TList(contained) => contained
        case TNode(_) => TAny
        case _ =>
          error(s"Cannot access field `children` of type $receiverTy", exp)
          TAny
      }
    case NextLink => TAny
    case PreviousLink => TAny
    case SizeLink =>
      receiverTy match {
        case TList(_) => TScalaInt
        case _ =>
          error(s"Cannot access field `size` of non-list type in $receiverTy", exp)
          TScalaInt
      }
  }

  def typecheckCall(fun: PatternFunction, args: Seq[Expression], transitive: Boolean, exp: Expression): Type = {
    val name = fun.name

    if (fun.params.size != args.size) {
      error(s"Function $name expects ${fun.params.size} arguments, but found ${args.size} arguments in call", exp)
    }

    fun.params.zipAll(args, null, null) foreach {
      case (null, arg) =>
        typecheck(arg)
      case (param, null) =>
      // nothing
      case (param, arg) =>
        val argTy = typecheck(arg)
        if (meet(param.typ, argTy, dataModel) == TNothing) {
          warn(s"Cast of argument type $argTy to unrelated parameter type ${param.typ} will always fail", arg)
        }
    }

    if (transitive) {
      // TODO
    }

    fun.outParams match {
      case Seq() => TUnit
      case Seq(out) => out
      case outs => TTuple(outs)
    }
  }

  /**
   * Computes the result type of an Eval expression and validates the contained Scala code for type correctness
   */
  def typecheckEval(code: Scala[meta.Term], exp: Eval): Type = {
    // collect bound name and set free vars (params) of eval expression if not already set
    val params = exp.params match {
      case Some(params) => params
      case None =>
        val params = (CollectFreeScalaVars.freeVars(code.tree)
          .diff(boundNames.map(Name).toSet))
          .intersect(getBindings.keySet)
        exp.params = Some(params.map(EvalParam).toSeq)
        exp.params.get
    }

    // here we use a little hack. We create one big block that defines all the params with their type.
    // The initializing value is irrelevant.
    val paramString = params.flatMap { param =>
      lookupVar(param.name) match {
        case Some((decl,ty)) =>
          resolveTarget(param)(decl)
          assignType(param)(ty)
          import meta._
          Some(q"val ${Pat.Var(Term.Name(param.name.name))}: ${ty.asScala} = Predef.???".syntax)
        case None => None
      }
    }.mkString(";\n")

    val codeSource = s"{$paramString;\n${code.syntax}}"
    typecheckDecodeScala(codeSource, exp)
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

  def assignType(term: Typeable[Type] with SourceLocation)(computeType: => Type): Type = {
    val inferred = computeType
    term.typ match {
      case Some(annotated) =>
        if (!subtype(inferred, annotated, dataModel))
          error(s"Inferred type $inferred, but expected annotated type $annotated", term)
        annotated
      case None =>
        term.typed(inferred)
        inferred
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

  // type operations

  def subtype(ty1: Type, ty2: Type, dataModel: DataModel): Boolean = {
    val meetTy = meet(ty1, ty2, dataModel)
    meetTy == ty1
  }

  def resolvedType(ty: Type): Type = ty match {
    case node: TNode => node.target match {
      case Some(value) => value
      case None => throw new IllegalArgumentException(s"Node type ${ty} is not bound")
    }
    case TList(contained) => TList(resolvedType(contained).asInstanceOf[TLinked])
    case TEnumeration(contained) => TEnumeration(resolvedType(contained).asInstanceOf[TLinked])
    case TTuple(ts) => TTuple(ts.map(resolvedType))
    case TAny => ty
    case TNothing => ty
    case TLiteral(_) => ty
    case TScala(_) => ty
  }

  protected def meet(ty1: Type, ty2: Type, dataModel: DataModel): Type = (ty1, ty2) match {
    case (_, _) if ty1 == ty2 => ty1
    case (TAny, _) => ty2
    case (_, TAny) => ty1
    case (TAnyLinked, _:TLinked) => ty2
    case (_:TLinked,TAnyLinked) => ty1
    case (node1: TNode, node2: TNode) =>
      val name1 = node1.target.getOrElse(throw new IllegalArgumentException(s"Unbound node type ${node1}")).name
      val name2 = node2.target.getOrElse(throw new IllegalArgumentException(s"Unbound node type ${node2}")).name
      if (name1 == name2)
        ty1
      else if (dataModel.nodeSupertypes.containsEntry(SortType(name1) -> SortType(name2)))
        ty1
      else if (dataModel.nodeSupertypes.containsEntry(SortType(name2) -> SortType(name1)))
        ty2
      else
        TNothing
    case (TList(s1), TList(s2)) => TList(meet(s1, s2, dataModel).asInstanceOf[TLinked])
    case (TTuple(tys1), TTuple(tys2)) if tys1.size == tys2.size => TTuple(tys1.zip(tys2).map(tt => meet(tt._1, tt._2, dataModel)))
    case (TScala(s1), TScala(s2)) =>
      if (subtypeScala(s1.tree, s2.tree))
        ty1
      else if (subtypeScala(s2.tree, s1.tree))
        ty2
      else
        TNothing
    case (_, TScala(s2)) =>
      if (subtypeScala(ty1.asScala, s2.tree))
        ty1
      else
        TNothing
    case (TScala(s1), _) =>
      if (subtypeScala(s1.tree, ty2.asScala))
        ty2
      else
        TNothing
    case _ => TNothing
  }

  protected def truechangeTypeToType(ty: truechange.Type): Type = ty match {
    case AnyType => TAny
    case SortType(name) => TNode(name)
    case ListType(ty) =>
      val convertedTy = truechangeTypeToType(ty)
      convertedTy match {
        case linked: TLinked => TList(linked)
        case _ => throw new IllegalArgumentException()
      }
    case _ => throw new UnsupportedOperationException(s"conversion of $ty from truechange to inca not supported")
  }

  protected def stmMeet(stmTy1: StmType, stmTy2: StmType, lang: DataModel): StmType = (stmTy1, stmTy2) match {
    case (NoYield, _) => NoYield
    case (_, NoYield) => NoYield
    case (Yields(ty1), Yields(ty2)) => Yields(meet(ty1, ty2, lang))
  }
}
