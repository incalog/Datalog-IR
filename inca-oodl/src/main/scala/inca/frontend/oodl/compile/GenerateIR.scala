package inca.frontend.oodl.compile

import inca.frontend.oodl.compile.GenerateIR.*
import inca.frontend.oodl.foreign.OODLAggregationOperator
import inca.frontend.oodl.syntax.*
import inca.frontend.oodl.util.ParseUtil
import inca.ir
import inca.ir.{Arg, ExtensionalRelation, Language, Name, RefByName, TermArg, WildcardArg, name2string, string2name}
import inca.ir.extension.aggregate as iragg
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.extension.aggregateset as iraggset
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.block
import inca.ir.extension.bool
import inca.ir.extension.data as irdata
import inca.ir.extension.datamatch as irmatch
import inca.ir.extension.demand
import inca.ir.extension.demand.demandRelationName
import inca.ir.extension.disjunction
import inca.ir.extension.disjunction.DisjunctionAlternative
import inca.ir.extension.not as irnot
import inca.ir.extension.set as irset
import inca.ir.extension.string as irstring
import inca.ir.extension.tuple as irtuple
import inca.ir.extension.impure as irimpure
import inca.ir.extension.mono as irmono
import inca.ir.extension.impure
import inca.util.Gensym
import inca.frontend.oodl.syntax.Type.signatureString

// TODO: Classes with same method name, but different params names that do not inherit from
//  each other do not work, because dynamic dispatch only includes signature, but not the name of the base class
// TODO: Use gensym everywhere to prevent name collision
// TODO: Support NullPointer error (If method call recv is null write it to a special relation)
// TODO: Support SetFold
// TODO: Subtyping of method arguments on override
// TODO: Generics
// TODO: Pattern matching
// TODO: Support mono-types

case object AllocImpurityKind extends irimpure.ImpurityKind:
  val name: String = "Alloc"
  val ty: ir.Type = irarith.TInt

case object MutationImpurityKind extends irimpure.ImpurityKind:
  val name: String = "Mutation"
  val ty: ir.Type = irarith.TInt

object GenerateIR:
  def subtypeRelationName = "subtype$"
  def castRelationName = "cast$"
  def extensionalRelationPrefix = "ext_"
  def extensionalRelationName(name: String): String = extensionalRelationPrefix + demandRelationName(name)

class GenerateIR:
  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + bool.IR + irdata.IR + irmatch.IR
    + demand.IR + disjunction.IR + irnot.IR + irset.IR + irstring.IR + irtuple.IR
    + iragg.IR + iraggset.IR + irimpure.IR
  )

  val gensym: Gensym = new Gensym()

  var builtinIdDatastructures: Seq[irdata.DataModuleEntry] = Seq()
  var setFoldRelations: Seq[ir.Relation] = Seq()

  def compileModule(m: Module): ir.Module =
    setFoldRelations = Seq()

    val mainFunctions = m.content.flatMap {
      case f: FunctionDef if f.isMain => Some(f)
      case _ => None
    }
    val extMainInputRelations = mainFunctions.map { f =>
      val name = extensionalRelationName(f.name)
      val allocInParam = ir.Param(AllocImpurityKind.name, AllocImpurityKind.ty)
      val mutInParam = ir.Param(MutationImpurityKind.name, MutationImpurityKind.ty)
      val monoInParam = ir.Param(irmono.MonoImpurityKind.name, irmono.MonoImpurityKind.ty)
      val params = f.params.map(p => ir.Param(p.name, compileType(p.typ)))
      ExtensionalRelation(name, params :+ allocInParam :+ mutInParam :+ monoInParam)
    }

    val classes = m.classes
    builtinIdDatastructures = compileDatastructures(classes)
    val clsHierarchyRelation = compileClassHierarchy(classes)
    val dispatchRelations = compileMethodsAndDispatchTable(classes)
    val objClass = compileBuiltinObjectClass()

    val moduleEntries = m.content.flatMap {
      case f: FunctionDef if f.isMain => Seq(compileMainFunction(f))
      case f: FunctionDef => Seq() // Skip all none main functions. We just use them for set fold
      case c: ClassDef => compileClassDef(c)
    } ++ extMainInputRelations

    val castRelation = compileCastRelation()
    val builtinContent = builtinIdDatastructures ++ Seq(objClass, castRelation)

    ir.Module(
      m.name,
      irLang,
      (builtinContent ++ moduleEntries ++ dispatchRelations ++ setFoldRelations) :+ clsHierarchyRelation
    )

  /** Helper */

  private def matchRuntimeType(t: ir.Term, tyTerm: ir.Term): ir.Atom =
    disjunction.Disjunction(
      builtinIdDatastructures.flatMap {
        case irdata.CaseDefinition(name, args, _) =>
          val wildcardArgs = (0 until args.size - 1).map(_ => WildcardArg())
          val deconstr = irdata.Deconstruct(t, RefByName(name), tyTerm.arg +: wildcardArgs, false)
          Some(DisjunctionAlternative(deconstr))
        case _ => None
      }
    )

  /** Module content */

  def compileMainFunction(f: FunctionDef): ir.Relation =
    val result = gensym.fresh(f.name.name + "_result")
    val setMember = f.outType match
      case TSet(ty) => Some(irset.SetMember(ir.Var(Name(gensym.fresh("_"))), ir.Var(Name(result))))
      case _ => None
    val resultParam = ir.Param(result, compileType(f.outType))
    val params = f.params.map(p => ir.Param(p.name, compileType(p.typ))) :+ resultParam
    val allocInVar = Name(gensym.fresh("ext_" + AllocImpurityKind.name))
    val mutInVar = Name(gensym.fresh("ext_" + MutationImpurityKind.name))
    val monoInVar = Name(gensym.fresh("ext_" + irmono.MonoImpurityKind.name))
    val inArgs = f.params.map(p => ir.Var(p.name).arg)
    val edbInputCall = ir.ExtensionalCall(extensionalRelationName(f.name),
      inArgs :+ ir.Var(allocInVar).arg :+ ir.Var(mutInVar).arg :+ ir.Var(monoInVar).arg)
    // set the first impure input to the edb input
    val impureAllocIn = irimpure.Impure(allocInVar, Seq(), ir.Var(allocInVar), AllocImpurityKind)
    val impureMutIn = irimpure.Impure(mutInVar, Seq(), ir.Var(mutInVar), MutationImpurityKind)
    val impureMonoIn = irimpure.Impure(mutInVar, Seq(), ir.Var(mutInVar), irmono.MonoImpurityKind)

    ir.Relation(f.name, params, Seq(ir.Body(
      (edbInputCall +: impureAllocIn +: impureMutIn +: impureMonoIn +: compileStatements(f.body, result)) ++ setMember
    ))).addHint(impure.PureHint)

  def compileCastRelation(): ir.Relation =
    val runtimeTyp = ir.Var("ty")
    ir.Relation(castRelationName,
      Seq(
        ir.Param("this", demand.TDemand(irdata.TData("ID"))),
        ir.Param("type", demand.TDemand(irstring.TString))
      ), Seq(ir.Body(Seq(
        matchRuntimeType(ir.Var("this"), runtimeTyp),
        ir.Call(subtypeRelationName, Seq(runtimeTyp.arg, ir.Var("type").arg))
      ))))

  def compileDatastructures(classDefs: Seq[ClassDef]): Seq[irdata.DataModuleEntry] =
    val data = irdata.DataDefinition("ID")
    val caseClassFields = classDefs.filter(_.isCaseClass).map(c => c.name -> c.fields)
    val sidCases = caseClassFields.map {
      case (name, fields) =>
        val signature = fields.map(_.typ)
        val qualifiedName = s"SID$$${signatureString(signature)}"
        irdata.CaseDefinition(qualifiedName, irstring.TString +: signature.map(compileType), irdata.TData(data.name))
    }.distinct
    val oidCase = irdata.CaseDefinition("OID", Seq(irstring.TString, AllocImpurityKind.ty), irdata.TData(data.name))
    data +: oidCase +: sidCases

  def compileBuiltinObjectClass(): ir.Relation =
    ir.Relation("Object", Seq(ir.Param("this", demand.TDemand(irdata.TData("ID")))), Seq(ir.Body(Seq())))
      .addHint(impure.PureHint)

  def compileClassHierarchy(classes: Seq[ClassDef]): ir.Relation =
    val noneTransitiveSubtypeTuples = classes.flatMap { c =>
      c.parentCls.map {
        case p: TName => (c.name.name, p.name.name)
        case t => throw IllegalStateException(s"Unexpected parent class type $t")
      } :+ ("Null", c.name.name)
    }.distinct
    ir.Relation(
      subtypeRelationName,
      Seq(ir.Param("ty1", irstring.TString), ir.Param("ty2", irstring.TString)),
      noneTransitiveSubtypeTuples.map { case (ty1, ty2) =>
        ir.Body(Seq(
          ir.Eq(ir.Var("ty1"), irstring.StringLit(ty1)),
          ir.Eq(ir.Var("ty2"), irstring.StringLit(ty2))
        ))
      } :+ ir.Body(Seq(
        ir.Call(subtypeRelationName, Seq(ir.Var("ty1").arg, ir.Var("ty").arg)),
        ir.Call(subtypeRelationName, Seq(ir.Var("ty").arg, ir.Var("ty2").arg))
      )) :+ ir.Body(Seq(
        ir.Call(subtypeRelationName, Seq(ir.Var("ty1").arg, ir.Var("_$0").arg)),
        ir.Eq(ir.Var("ty2"), ir.Var("ty1"))
      ))
    ).addHint(impure.PureHint)

  def compileClassDef(c: ClassDef): Seq[ir.Relation] =
    val fieldRelations = c.fields.flatMap(compileFieldDef)
    val constructorRelations = c.constructors.map(compileConstructorDef(_)(c))
    // Methods are handled globally by `compileMethodsAndDispatchTable`
    fieldRelations ++ constructorRelations

  /** Class content */

  /** Transitively collect all methods for a given qualified name */
  private def collectMethods(classDef: ClassDef)(implClass: ClassDef = classDef): Map[String, (ClassDef, MethodDef)] = {
    val methods = implClass.methods.map { m =>
      val qualifiedMethodName = s"${m.name}$$${signatureString(m.signature)}"
      qualifiedMethodName -> (implClass, m)
    }.toMap

    val parentMethods = implClass.parentCls.flatMap {
      case t: TName if !t.isBuiltIn => t.target match
        case Some(parentClassDef: ClassDef) => collectMethods(classDef)(parentClassDef)
        case _ => throw IllegalStateException(s"Unresolved ClassDef ${t.name}")
      case t => throw IllegalStateException(s"Unexpected type $t")
    }.toMap
    // We rely on the default map collision behaviour to find the concrete implementation class
    parentMethods ++ methods
  }

  def compileMethodsAndDispatchTable(classes: Seq[ClassDef]): Seq[ir.Relation] =
    val collectedMethods = classes.map(c => c -> collectMethods(c)()).toMap
    // qualifiedMethodName -> (src1, trg1), ...,(srcN, trgN)
    var dispatchClasses: Map[String, Seq[(String, String)]] = Map()
    // qualifiedMethodName -> MethodDef1, ..., MethodDefN
    var qualifiedMethods: Map[String, Seq[MethodDef]] = Map()
    classes.foreach { c =>
      collectedMethods(c).foreach { case (qualifiedMethodName, (implClass, implMethod)) =>
        val previousTuples = dispatchClasses.getOrElse(qualifiedMethodName, Seq())
        dispatchClasses += qualifiedMethodName -> (previousTuples :+ (c.name.name, implClass.name.name))
        val previousMethods = qualifiedMethods.getOrElse(qualifiedMethodName, Seq())
        qualifiedMethods += qualifiedMethodName -> (previousMethods :+ implMethod)
      }
    }

    val dispatchTables = dispatchClasses.map { case (qualifiedMethodName, srcAndTrg) =>
      ir.Relation(
        s"dispatch$$$qualifiedMethodName",
        Seq(ir.Param("src", irstring.TString), ir.Param("trg", irstring.TString)),
        srcAndTrg.map { case (src, trg) =>
          ir.Body(Seq(
            ir.Eq(ir.Var("src"), irstring.StringLit(src)),
            ir.Eq(ir.Var("trg"), irstring.StringLit(trg)),
          ))
        }
      ).addHint(impure.PureHint)
    }.toSeq

    dispatchTables ++ qualifiedMethods.map((q, ms) => compileMethodDefs(q, ms)).toSeq

  def compileMethodDefs(qualifiedName: Name, methods: Seq[MethodDef]): ir.Relation = //gensym.scoped {
    val reprMethod = methods.head
    val thisParam = ir.Param("this", demand.TDemand(irdata.TData("ID")))
    val classGuardParam = ir.Param(gensym.freshName("param"), demand.TDemand(irstring.TString))
    val params = reprMethod.params.map(p => ir.Param(p.name, demand.TDemand(compileType(p.typ))))
    val resultParam = ir.Param(gensym.freshName("return"), compileType(reprMethod.outType))

    // Find all distinct classes that implement the method
    val methodsWithImplClass = methods.map(m =>
      m.target match
        case Some(cls: ClassDef) => cls -> m
        case _ => throw IllegalStateException(s"Unresolved ClassRef for method ${m.name}")
    ).toMap

    val tmpResult = gensym.freshName("return")

    ir.Relation(
      qualifiedName,
      classGuardParam +: (thisParam +: (params :+ resultParam)),
      methodsWithImplClass.map { case (implClass, m) =>
        val body = compileStatements(m.body, tmpResult)
        val classGuard = ir.Eq(ir.Var(classGuardParam.name), irstring.StringLit(implClass.name))
        val castedResult = ir.Eq(ir.Var(resultParam.name), ir.Cast(ir.Var(tmpResult), compileType(reprMethod.outType)))
        ir.Body(classGuard +: body :+ castedResult)
      }.toSeq
    )
  //}

  def compileConstructorDef(c: ConstructorDef)(classDef: ClassDef): ir.Relation =
    val className = c.target match
      case Some(c: ClassDef) => c.name
      case _ => throw IllegalStateException(s"Unresolved class for constructor.")
    val thisParam = ir.Param("this", demand.TDemand(irdata.TData("ID")))
    val params = c.params.map(p => ir.Param(p.name, demand.TDemand(compileType(p.typ))))
    val unusedResultVar = gensym.freshName("_")
    // Remove all assignment of inherited fields
    val body = c.body.flatMap {
      case s@Assign(select@Select(recv, targetName), Name("="), rhs) =>
        select.target match
          case Some((clsDef, _)) if clsDef.name != className => None
          case _ => Some(s)
      case s => Some(s)
    }
    // collect all none-generated fields an assign their initial value
    val assignUserFields = classDef.fields.filter(!_.isGeneratedConstructorField).map {
      case f: FieldDef if f.body.isEmpty => throw IllegalStateException(s"Encountered unassigned field ${f.name}")
      case f: FieldDef if f.immutable =>
        ir.Call(s"${classDef.name}$$$$${f.name}", Seq(ir.Var("this").arg, compileExpression(f.body.get).arg))
      case f: FieldDef if !f.immutable =>
        ir.Call(s"${classDef.name}$$$$${f.name}", Seq(ir.Var("this").arg, compileExpression(f.body.get).arg, irarith.IntNum(0).arg))
    }
    ir.Relation(className, thisParam +: params, Seq(ir.Body(compileStatements(body, unusedResultVar) ++ assignUserFields)))
      //.addHint(Hints.Pure)

  // Prevent compiling inherited fields multiple times
  var visitedFields: Set[(ClassDef, Name)] = Set.empty

  def compileFieldDef(f: FieldDef): Seq[ir.Relation] =
    // Find the original definition based on the inherited one
    val classDef = f.target match
      case Some(cls) => cls
      case _ => throw IllegalStateException(s"Unresolved ClassDef target for field ${f.name}")
    val fieldSuperTy = classDef.fields.find(_.name == f.name).get.typ

    if (visitedFields.contains((classDef, f.name)))
      Seq()
    else
      val rels = compileFieldDefInternal(f, classDef, fieldSuperTy)
      visitedFields += (classDef, f.name)
      rels

  def compileFieldDefInternal(f: FieldDef, classDef: ClassDef, fieldTy: Type): Seq[ir.Relation] =
    val qualifiedName = s"${classDef.name}$$$$${f.name}"
    val thisParam = ir.Param("this", demand.TDemand(irdata.TData("ID")))
    val valueParam = ir.Param("value", demand.TDemand(compileType(fieldTy)))
    if (f.immutable)
      Seq(ir.Relation(qualifiedName, Seq(thisParam, valueParam), Seq(ir.Body(Seq()))))
    else
      val tsParam = ir.Param("ts", demand.TDemand(MutationImpurityKind.ty)) //ir.Param("ts", demand.TDemand(Mutation.ty))
      val fieldRel = ir.Relation(qualifiedName, Seq(thisParam, valueParam, tsParam), Seq(ir.Body(Seq())))
      // Create filter relation
      val filterRelName = s"$qualifiedName$$Filter"
      val tsMaxParam = ir.Param("aggTs", MutationImpurityKind.ty)
      val filterRel = ir.Relation(filterRelName, Seq(thisParam, tsParam, tsMaxParam), Seq(
        ir.Body(Seq(
          ir.Call(qualifiedName, Seq(ir.Var("this").arg, ir.WildcardArg(), ir.Var("aggTs").arg))
            .addHint(demand.DemandIgnoreCallHint),
          irarith.LT(ir.Var("aggTs"), ir.Var("ts"))
        )))).addHint(impure.PureHint)

      val maxTs = ir.Var(gensym.fresh("maxTs"))
      val mutVar = Name(gensym.fresh("current" + MutationImpurityKind.name))
      val fieldRead = ir.Relation(s"$qualifiedName$$Read", Seq(thisParam, ir.Param("value", compileType(f.typ))), Seq(ir.Body(Seq(
        irimpure.Impure(mutVar, Seq(
          iragg.Aggregate(
            RefByName(filterRelName),
            Seq(ir.Var("this").arg, ir.Var(mutVar).arg, iragg.AggregateColumnArg(maxTs)),
            irarith.ArithmeticAggregationOperator.MaxInt
          ),//.addHint(demand.Hints.IgnoreCall),
          ir.Call(qualifiedName, Seq(ir.Var("this").arg, ir.Var("value").arg, maxTs.arg))
            .addHint(demand.DemandIgnoreCallHint)
        ), ir.Var(mutVar), MutationImpurityKind)
      ))))

      Seq(fieldRel, filterRel, fieldRead)

  /** Statement */

  def compileStatements(stmts: Seq[Statement], resultVar: Name): Seq[ir.Atom] = stmts match
    case Nil => Seq()
    case (stm@Return(_)) :: _ => Seq(compileStatement(stm, resultVar))
    case (stm@If(cnd, thn, els)) :: rest =>
      // Note: This assumes, that all VarPhiAssigns directly follow an if stmt
      val (varPhiAssigns, remainingStmts) = rest.span {
        case VarPhiAssign(name, typ, ifStmt, _, _) => stm == ifStmt
        case _ => false
      }
      // Merge all VarPhiAssigns into the thn and els branch
      val (thnDeclarations, elsDeclarations) = varPhiAssigns.map {
        case VarPhiAssign(name, typ, _, thnName, elsName) =>
          val thnDecl = VarDeclare(name, Some(typ), Some(Var(thnName)), true)
          val elsDecl = VarDeclare(name, Some(typ), Some(Var(elsName)), true)
          (thnDecl, elsDecl)
      }.unzip
      // Merge remaining stmts to correctly handle return
      // Note: This generates a lot of duplicated atoms
      val thnStmts = thn ++ thnDeclarations ++ remainingStmts
      val elsStmts = els ++ elsDeclarations ++ remainingStmts
      val ifAtom = compileStatement(If(cnd, thnStmts, elsStmts), resultVar)
      Seq(ifAtom)
    case stm :: rest => compileStatement(stm, resultVar) +: compileStatements(rest, resultVar)

  def compileStatement(stm: Statement, resultVar: Name): ir.Atom = stm match
    case Expr(expression) =>
      val wildcard = gensym.fresh("_")
      ir.Eq(ir.Var(wildcard), compileExpression(expression))
    case Return(expression) =>
      ir.Eq(ir.Var(resultVar), compileExpression(expression))
    case superCall@Super(args) =>
      val superClassName = superCall.target match
        case Some((c: ClassDef, _)) => c.name
        case _ => throw IllegalStateException(s"Unresolved target for super call '$superCall'")
      ir.Call(superClassName, ir.Var("this").arg +: args.map(compileExpression).map(_.arg))
    case Assign(select@Select(recv, targetName), Name("="), rhs) =>
      val (classDef, fieldDef) = select.target match
        case Some((c, f)) => c -> f
        case _ => throw IllegalStateException(s"Unresolved target for select $recv.$targetName")
      val qualifiedName = s"${classDef.name}$$$$${fieldDef.name}"
      val recvTerm = compileExpression(recv)
      val rhsTerm = compileExpression(rhs)
      if (fieldDef.immutable)
        ir.Call(qualifiedName, Seq(recvTerm.arg, rhsTerm.arg))
      else
        val mutVar = Name(gensym.freshName("current" + MutationImpurityKind.name))
        val fieldSetter = ir.Call(qualifiedName, Seq(recvTerm.arg, rhsTerm.arg, ir.Var(mutVar).arg))
        irimpure.Impure(mutVar, fieldSetter, irarith.Add(ir.Var(mutVar), irarith.IntNum(1)), MutationImpurityKind)
    case Assign(lhs, Name("="), rhs) =>
      ir.Eq(compileExpression(lhs), compileExpression(rhs))
    case Assign(lhs, Name("+="), rhs) =>
      irmono.WriteMono(compileExpression(lhs), compileExpression(rhs), Seq())
    case VarDeclare(name, typ, None, immutable) =>
      throw IllegalStateException(s"Can not compile variable declaration '$name' without a value")
    case VarDeclare(name, typ, _, false) =>
      throw IllegalStateException(s"Can not compile mutable variable '$name'")
    case VarDeclare(name, _, Some(expr), true) =>
      ir.Eq(ir.Var(name), compileExpression(expr))
    case If(cnd, thn, els) =>
      val cndTerm = compileExpression(cnd)
      disjunction.Disjunction(Seq(
        DisjunctionAlternative(
          ir.Eq(cndTerm, bool.BoolTrue) +: compileStatements(thn, resultVar)
        ),
        DisjunctionAlternative(
          ir.Eq(cndTerm, bool.BoolFalse) +: compileStatements(els, resultVar)
        )
      ))
    case VarPhiAssign(name, typ, If(cnd, _, _), thnName, elsName) =>
      throw IllegalStateException(s"Encountered unexpected VarPhiAssign for name: '$name'")

  /** Expression */

  private def generateSetFoldRelation(sty: Type): ir.Relation = {
    val TSet(ty) = sty
    val name = gensym.fresh("setCollect")
    ir.Relation(
      name,
      Seq(
        ir.Param("set", demand.TDemand(compileType(sty))),
        ir.Param("ele", compileType(ty))
      ),
      Seq(
        ir.Body(Seq(irset.SetMember(ir.Var("ele"), ir.Var("set"))))
      )
    )
  }

  def compileExpression(e: Expression): ir.Term = e.cast match
    case None => compileCastedExpression(e)
    case Some(trgTy) => ir.Cast(compileCastedExpression(e), compileType(trgTy))

  def compileCastedExpression(expr: Expression): ir.Term = expr match
    case NullLit() => irdata.Construct("OID", Seq(irstring.StringLit("Null"), irarith.IntNum(-1)))

    case BoolLit(b) => if (b) bool.BoolTrue else bool.BoolFalse
    case BinOp(e1, "&&", e2) => bool.BoolAnd(compileExpression(e1), compileExpression(e2))
    case BinOp(e1, "||", e2) => bool.BoolOr(compileExpression(e1), compileExpression(e2))

    case BinOp(e1, "==", e2) => bool.AtomAsBool(ir.Eq(compileExpression(e1), compileExpression(e2)))
    case BinOp(e1, "!=", e2) => bool.AtomAsBool(ir.Eq(compileExpression(e1), compileExpression(e2), true))

    case StringLit(s) => irstring.StringLit(s)
    case BinOp(e1, "+", e2) if expr.typ.contains(TString) =>
      irstring.StringConcat(compileExpression(e1), compileExpression(e2))

    case IntLit(i) => irarith.IntNum(i)
    case DoubleLit(d) => irarith.DoubleNum(d)
    case UnOp("!", e) => e.typ match
      case Some(TBoolean) => bool.BoolNot(compileExpression(e))
      case _ => throw new IllegalArgumentException(s"Cannot compile code of type ${e.typ}, $e")
    case UnOp("-", e) => e.typ match
      case Some(TInt) => irarith.Sub(irarith.IntNum(0), compileExpression(e))
      case Some(TDouble) => irarith.Sub(irarith.DoubleNum(0), compileExpression(e))
      case _ => throw new IllegalArgumentException(s"Cannot compile code of type ${e.typ}, $e")
    case BinOp(e1, "+", e2) => irarith.Add(compileExpression(e1), compileExpression(e2))
    case BinOp(e1, "*", e2) => irarith.Mul(compileExpression(e1), compileExpression(e2))
    case BinOp(e1, "-", e2) => irarith.Sub(compileExpression(e1), compileExpression(e2))
    case BinOp(e1, "/", e2) => irarith.Div(compileExpression(e1), compileExpression(e2))
    case BinOp(e1, "%", e2) => irarith.Remainder(compileExpression(e1), compileExpression(e2))
    case BinOp(e1, ">", e2) => bool.AtomAsBool(irarith.GT(compileExpression(e1), compileExpression(e2)))
    case BinOp(e1, ">=", e2) => bool.AtomAsBool(irarith.GE(compileExpression(e1), compileExpression(e2)))
    case BinOp(e1, "<", e2) => bool.AtomAsBool(irarith.LT(compileExpression(e1), compileExpression(e2)))
    case BinOp(e1, "<=", e2) => bool.AtomAsBool(irarith.LE(compileExpression(e1), compileExpression(e2)))

    case Var(name) => ir.Var(name)

    case select@Select(recv, targetName) =>
      recv.typ match
        // Project by parsing targetName
        case Some(t: TTuple) =>
          val index = ParseUtil.parseTupleIndex(targetName) match
            case Some(idx) => idx - 1
            case _ => throw IllegalStateException(s"Unexpected tuple index $targetName")
          irtuple.Project(compileExpression(recv), index)

        // FieldRead
        case Some(t: TName) =>
          val (classDef, fieldDef) = select.target match
            case Some((c, f)) => c -> f
            case _ => throw IllegalStateException(s"Unresolved target for select $recv.$targetName")
          val qualifiedName = s"${classDef.name}$$$$${fieldDef.name}"
          val recvTerm = compileExpression(recv)
          val resultVar = ir.Var(gensym.fresh(fieldDef.name))

          if (classDef.isMonoClass && fieldDef.name.name == "result")
            irmono.ReadMono(compileExpression(recv))
          else if (classDef.isCaseClass)
            // This is only okay, since case classes can not inherit fields. Otherwise we would need to match at runtime
            val allFields = classDef.fields
            val signature = allFields.map(_.typ)
            val fieldIndex = allFields.indexWhere(_.name == targetName)
            var args: Seq[Arg] = (0 until allFields.size + 1).map(_ => WildcardArg())
            args = args.updated(fieldIndex + 1, resultVar.arg)
            val caseName = Name(s"SID$$${signatureString(signature)}")
            block.Block(
              irdata.Deconstruct(recvTerm, RefByName(caseName), args, false),
              resultVar
            )
          else if (fieldDef.immutable)
            block.Block(
              ir.Call(qualifiedName, Seq(recvTerm.arg, resultVar.arg)).addHint(demand.DemandIgnoreCallHint),
              resultVar
            )
          else
            block.Block(
              ir.Call(s"$qualifiedName$$Read", Seq(recvTerm.arg, resultVar.arg)),//.addHint(demand.Hints.IgnoreCall),
              resultVar
            )
        case _ => throw IllegalStateException(s"Cannot compile select from receiver type ${recv.typ}, $recv")

    case constrCall@ConstructorCall(name, _, args) =>
      val (classDef, constrDef) = constrCall.target match
        case Some((c, constr)) => (c, constr)
        case _ => throw IllegalStateException(s"Unresolved target for constructor call '$constrCall'")
      classDef match {
        case cls if cls.isCaseClass =>
          // Note: This assumes the constructor args and fields are ordered the same way
          val caseName = s"SID$$${signatureString(constrDef.signature)}"
          val caseArgs = irstring.StringLit(classDef.name) +: args.map(compileExpression)
          irdata.Construct(caseName, caseArgs)
        case cls if cls.isMonoClass =>
          cls.name match
            case Name("mono.Count") => irmono.NewMono(irmono.ArithmeticMonoDefinition.Count, Seq(), Seq())
            case _ => ???
        case _ =>
          val oidVar = ir.Var(gensym.fresh("oid"))
          val allocVar = Name(gensym.freshName("current" + AllocImpurityKind.name))
          val caseArgs = Seq(irstring.StringLit(classDef.name), ir.Var(allocVar))
          val dataConstr = irdata.Construct("OID", caseArgs)
          block.Block(Seq(
            irimpure.Impure(allocVar, ir.Eq(oidVar, dataConstr), irarith.Add(ir.Var(allocVar), irarith.IntNum(1)), AllocImpurityKind),
            ir.Call(name, oidVar.arg +: args.map(compileExpression).map(_.arg)),
          ), oidVar)
      }

    case methodCall@MethodCall(recv, Name("fold"), _, args, _) if recv.typ.exists(_.isInstanceOf[TSet]) =>
      val Seq(init, op: Var) = args: @unchecked
      val f = op.target match
        case Some(f: FunctionDef) => f
        case trg => throw new IllegalArgumentException(s"Cannot compile fold with non-function op target $trg")
      val aggOp = OODLAggregationOperator(f, init, op)

      val aggResult = Name(gensym.fresh("foldResult"))
      val aggArgs = Seq(compileExpression(recv).arg, iragg.AggregateColumnArg(ir.Var(aggResult)))

      val setFoldRel = generateSetFoldRelation(recv.typ.get)
      setFoldRelations :+= setFoldRel

      val demandSet = ir.Call(setFoldRel.name, Seq(aggArgs.head, ir.WildcardArg()))
      val agg = iragg.Aggregate(RefByName(setFoldRel.name), aggArgs, aggOp).addHint(demand.DemandIgnoreCallHint)
      block.Block(Seq(demandSet, agg), ir.Var(aggResult))

    case methodCall@MethodCall(recv, fun, _, args, isFix) =>
      recv.typ match
        case Some(t: TName) if t.isBuiltIn =>
          fun.name match
            case "toString" => irstring.ToString(compileExpression(recv))
            case _ => throw IllegalStateException(s"Can not compile builtin method $fun on primitive $recv")
        case Some(t: TName) =>
          val (_, methodDef) = methodCall.target match
            case Some((c, m)) => (c, m)
            case _ => throw IllegalStateException(s"Unresolved target for method call '$methodCall'")
          val qualifiedMethodName = s"${methodDef.name}$$${signatureString(methodDef.signature)}"
          val dispatchName = s"dispatch$$$qualifiedMethodName"
          val srcClsVar = ir.Var(gensym.fresh("C"))
          val trgClsVar = ir.Var(gensym.fresh("D"))
          val recvTerm = compileExpression(recv)
          val resultVar = ir.Var(gensym.fresh("return$"))
          block.Block(Seq(
            matchRuntimeType(recvTerm, srcClsVar),
            ir.Call(dispatchName, Seq(srcClsVar.arg, trgClsVar.arg)),
            ir.Call(qualifiedMethodName, trgClsVar.arg +: (compileExpression(recv).arg +: args.map(compileExpression).map(_.arg)) :+ resultVar.arg)
          ), resultVar)
        case _ => throw IllegalStateException(s"Can not compile method $fun on object $recv")
    case TypeCast(recv, toTyp: TName) if toTyp.isBuiltIn =>
      throw IllegalStateException(s"Can not typecast to builtin type $toTyp")
    case TypeCast(recv, toTyp: TName) =>
      val recvTerm = compileExpression(recv)
      block.Block(
        ir.Call(castRelationName, Seq(recvTerm.arg, irstring.StringLit(toTyp.toString).arg)),
        ir.Cast(recvTerm, compileType(toTyp))
      )
    case TypeCast(recv, toTyp) =>
      throw IllegalStateException(s"Can not typecast to type $toTyp")
    case InstanceOf(recv, t: TName) if t.isBuiltIn =>
      throw IllegalStateException(s"Can not check instance of builtin type $t")
    case InstanceOf(recv, t: TName) =>
      val srcClsVar = ir.Var(gensym.fresh("C"))
      val recvTerm = compileExpression(recv)
      val resultVar = ir.Var(gensym.fresh("isInstanceOf$"))
      block.Block(Seq(
        matchRuntimeType(recvTerm, srcClsVar),
        disjunction.Disjunction(
          Seq(
            ir.Call(subtypeRelationName, Seq(srcClsVar.arg, irstring.StringLit(t.name).arg)),
            ir.Eq(resultVar, bool.BoolTrue)
          ),
          Seq(
            ir.Call(subtypeRelationName, Seq(srcClsVar.arg, irstring.StringLit(t.name).arg), true),
            ir.Eq(resultVar, bool.BoolFalse)
          ),
        )
      ), resultVar)
    case InstanceOf(recv, t) =>
      throw IllegalStateException(s"Can not check instance of type $t")
    case TupleExp(exps) => irtuple.TupleLit(exps.map(compileExpression))

    case BinOp(e1, "++", e2) => // set union
      irset.SetUnion(compileExpression(e1), compileExpression(e2))
    case BinOp(e1, "&", e2) => // set intersection
      irset.SetIntersection(compileExpression(e1), compileExpression(e2))
    case SetExp(exps, tty) =>
      val setLit = irset.SetLit(exps.map(compileExpression))
      tty match
        case Some(ty) => ir.Cast(setLit, compileType(TSet(ty)))
        case _ => setLit
    case SetMember(name, recv, predicate) =>
      val cond = predicate.map(compileExpression).getOrElse(bool.BoolTrue)
      block.Block(irset.SetMember(ir.Var(name), compileExpression(recv)), cond)
    case SetComprehension(member, body) =>
      val memberTerms = member.map(compileExpression)
      val bodyTerm = compileExpression(body)
      irset.SetComprehension(bodyTerm, memberTerms.map(t => ir.Eq(t, bool.BoolTrue)))
    case _ =>
      throw IllegalStateException(s"Unhandled expression $expr of class ${expr.getClass}")

  /** Type */

  def compileType(ty: Type): ir.Type = ty match
    case TAny => ir.TAny
    case TTuple(ts) => irtuple.TTuple(ts.map(compileType))
    case TSet(ty) => irset.TSet(compileType(ty))
    case TInt => irarith.TInt
    case TDouble => irarith.TDouble
    case TBoolean => bool.TBoolean
    case TString => irstring.TString
    case TNull | _: TName => irdata.TData("ID")
