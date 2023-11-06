package inca.frontend.oodl.compile

import inca.frontend.oodl.compile.GenerateIR.*
import inca.frontend.oodl.syntax.*
import inca.frontend.oodl.util.ParseUtil
import inca.ir
import inca.ir.{ExtensionalRelation, Language, Name, name2string, string2name}
import inca.ir.extension.aggregate as iragg
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
import inca.util.Gensym

case object Alloc extends irimpure.ImpurityKind:
  val name: String = "Alloc"
  val ty: ir.Type = irarith.TInt

case object Mutation extends irimpure.ImpurityKind:
  val name: String = "Mutation"
  val ty: ir.Type = irarith.TInt

object GenerateIR:
  def signatureString(tys: Seq[Type]): String = tys.map(_.signatureString).mkString("$")
  def subtypeRelationName = "subtype$"
  def extensionalRelationPrefix = "ext_"
  def extensionalRelationName(name: String): String = extensionalRelationPrefix + demandRelationName(name)

class GenerateIR:
  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + bool.IR + irdata.IR + irmatch.IR
    + demand.IR + disjunction.IR + irnot.IR + irset.IR + irstring.IR + irtuple.IR
    + iragg.IR + iraggset.IR + irimpure.IR
  )

  val gensym: Gensym = new Gensym()

  var builtInIdDatastructures: irdata.DataDefinition = null

  def compileModule(m: Module): ir.Module =
    val mainFunctions = m.content.flatMap {
      case f: FunctionDef if f.annos.exists(_.isInstanceOf[MainFunctionAnno]) => Some(f)
      case _ => None
    }
    val extMainInputRelations = mainFunctions.map { f =>
      val name = extensionalRelationName(f.name)
      val params = f.params.map(p => ir.Param(p.name, compileType(p.typ)))
      ExtensionalRelation(name, params)
    }

    val classes = m.classes
    builtInIdDatastructures = compileDatastructures(classes)
    val clsHierarchyRelation = compileClassHierarchy(classes)
    val dispatchRelations = compileMethodsAndDispatchTable(classes)
    val objClass = compileBuiltinObjectClass()

    val moduleEntries = m.content.flatMap {
      case f: FunctionDef if f.annos.exists(_.isInstanceOf[MainFunctionAnno]) => Seq(compileMainFunction(f))
      case f: FunctionDef => throw IllegalStateException(s"Can not compile none main function ${f.name}")
      case c: ClassDef => compileClassDef(c)
    } ++ extMainInputRelations

    ir.Module(
      m.name,
      irLang,
      (builtInIdDatastructures +: (moduleEntries ++ dispatchRelations)) :+ objClass :+ clsHierarchyRelation
    )

  /** Module content */

  def compileMainFunction(f: FunctionDef): ir.Relation =
    val result = gensym.fresh(f.name.name + "_result")
    val setMember = f.outType match
      case TSet(ty) => Some(irset.SetMember(ir.Var(Name(gensym.fresh("_"))), ir.Var(Name(result))))
      case _ => None
    val resultParam = ir.Param(Name(result), compileType(f.outType))
    val params = f.params.map(p => ir.Param(p.name, compileType(p.typ))) :+ resultParam
    val edbInputCall = ir.ExtensionalCall(extensionalRelationName(f.name), f.params.map(p => ir.Var(p.name)))
    ir.Relation(f.name, params, Seq(ir.Body(
      (edbInputCall +: compileStatements(f.body, Name(result))) ++ setMember
    )))

  def compileDatastructures(classDefs: Seq[ClassDef]): irdata.DataDefinition =
    val caseClassConstructors = classDefs.filter(_.isCaseClass).map(c => c.name -> c.constructors)
    val sidCases = caseClassConstructors.map {
      case (name, Seq(c)) => irdata.CaseDefinition(
        s"SID$$${signatureString(c.signature)}",
        irstring.TString +: c.signature.map(compileType)
      )
      case (name, _) => throw IllegalStateException(s"Found more than one Constructor for CaseClass '$name'")
    }
    val oidCase = irdata.CaseDefinition("OID", Seq(irstring.TString, Alloc.ty))
    val nullCase = irdata.CaseDefinition("NID", Seq(irstring.TString))

    irdata.DataDefinition("ID", (oidCase +: sidCases) :+ nullCase)

  def compileBuiltinObjectClass(): ir.Relation =
    ir.Relation("Object", Seq(ir.Param("this", demand.TDemand(irdata.TData("ID")))), Seq())

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
        ir.Call(subtypeRelationName, Seq(ir.Var("ty1"), ir.Var("ty"))),
        ir.Call(subtypeRelationName, Seq(ir.Var("ty"), ir.Var("ty2")))
      ))
    )

  def compileClassDef(c: ClassDef): Seq[ir.Relation] =
    val fieldRelations = c.fields.map(compileFieldDef)
    val constructorRelations = c.constructors.map(compileConstructorDef)
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
      )
    }.toSeq

    dispatchTables ++ qualifiedMethods.map((q, ms) => compileMethodDefs(q, ms)).toSeq

  def compileMethodDefs(qualifiedName: Name, methods: Seq[MethodDef]): ir.Relation = gensym.scoped {
    val reprMethod = methods.head
    val thisParam = ir.Param("this", demand.TDemand(irdata.TData("ID")))
    val classGuardParam = ir.Param(gensym.freshName("param"), irstring.TString)
    val params = reprMethod.params.map(p => ir.Param(p.name, demand.TDemand(compileType(p.typ))))
    val resultParam = ir.Param(gensym.freshName("return"), compileType(reprMethod.outType))

    // Find all distinct classes that implement the method
    val methodsWithImplClass = methods.map(m =>
      m.target match
        case Some(cls: ClassDef) => cls -> m
        case _ => throw IllegalStateException(s"Unresolved ClassRef for method ${m.name}")
    ).toMap

    ir.Relation(
      qualifiedName,
      classGuardParam +: (thisParam +: (params :+ resultParam)),
      methodsWithImplClass.map { case (implClass, m) =>
        val body = compileStatements(m.body, resultParam.name)
        val classGuard = ir.Eq(ir.Var(classGuardParam.name), irstring.StringLit(implClass.name))
        ir.Body(classGuard +: body)
      }.toSeq
    )
  }

  def compileConstructorDef(c: ConstructorDef): ir.Relation =
    val className = c.target match
      case Some(c: ClassDef) => c.name
      case _ => throw IllegalStateException(s"Unresolved ClassRef for constructor.")
    val thisParam = ir.Param("this", demand.TDemand(irdata.TData("ID")))
    val params = c.params.map(p => ir.Param(p.name, demand.TDemand(compileType(p.typ))))
    val unusedResultVar = gensym.freshName("_")
    ir.Relation(className, thisParam +: params, Seq(ir.Body(compileStatements(c.body, unusedResultVar))))

  def compileFieldDef(f: FieldDef): ir.Relation =
    val classDef = f.target match
      case Some(cls) => cls
      case _ => throw IllegalStateException(s"Unresolved ClassDef target for field ${f.name}")
    val qualifiedName = s"${classDef.name}$$$$${f.name}"
    val thisParam = ir.Param("this", demand.TDemand(irdata.TData("ID")))
    val valueParam = ir.Param("value", demand.TDemand(compileType(f.typ)))
    if (f.immutable)
      ir.Relation(qualifiedName, Seq(thisParam, valueParam), Seq())
    else {
      val tsParam = ir.Param("ts", demand.TDemand(Mutation.ty))
      ir.Relation(qualifiedName, Seq(thisParam, valueParam, tsParam), Seq())
    }

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
      val ifAtom = compileStatement(If(cnd, thn ++ thnDeclarations, els ++ elsDeclarations), resultVar)
      ifAtom +: compileStatements(remainingStmts, resultVar)
    case stm :: rest => compileStatement(stm, resultVar) +: compileStatements(rest, resultVar)

  def compileStatement(stm: Statement, resultVar: Name): ir.Atom = stm match
    case Expr(expression) =>
      val wildcard = gensym.fresh("_")
      ir.Eq(ir.Var(wildcard), compileExpression(expression))
    case Return(expression) =>
      ir.Eq(ir.Var(resultVar), compileExpression(expression))
    case Assign(Select(recv, targetName), rhs) =>
      val recvObject = compileExpression(recv)
      // TODO: Handle field read
      ???
    case Assign(lhs, rhs) =>
      ir.Eq(compileExpression(lhs), compileExpression(rhs))
    case VarDeclare(name, typ, None, immutable) =>
      throw IllegalStateException(s"Can not compile variable declaration '$name' without a value")
    case VarDeclare(name, typ, _, false) =>
      throw IllegalStateException(s"Can not compile mutable variable '$name'")
    case VarDeclare(name, typ, Some(expr), true) =>
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

  private def matchRuntimeType(t: ir.Term, tyTerm: ir.Term): ir.Atom =
    disjunction.Disjunction(
      builtInIdDatastructures.cases.map {
        case irdata.CaseDefinition(name, args) =>
          val wildcardArgs = (0 until args.size-1).map(_ => ir.Var(gensym.freshName("_")))
          val deconstr = irdata.Deconstruct(t, name, tyTerm +: wildcardArgs)
          DisjunctionAlternative(deconstr)
      }
    )

  def compileExpression(expr: Expression): ir.Term = expr match
    case NullLit() => irdata.Construct("NID", Seq(irstring.StringLit("Null")))

    case BinOp(e1, "==", e2) => bool.AtomAsBool(ir.Eq(compileExpression(e1), compileExpression(e2)))
    case BinOp(e1, "!=", e2) => bool.AtomAsBool(ir.Neq(compileExpression(e1), compileExpression(e2)))

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
        case Some(t: TTuple) => // Project by parsing targetName
          val index = ParseUtil.parseTupleIndex(targetName) match
            case Some(idx) => idx - 1
            case _ => throw IllegalStateException(s"Unexpected tuple index $targetName")
          irtuple.Project(compileExpression(recv), index)
        case Some(t: TName) => // FieldRead
          val (classDef, fieldDef) = select.target match
            case Some((c, f)) => c -> f
            case _ => throw IllegalStateException(s"Unresolved target for select $recv.$targetName")
          val qualifiedName = s"${classDef.name}$$$$${fieldDef.name}"
          val resultVar = ir.Var(gensym.fresh(fieldDef.name))
          if (fieldDef.immutable) {
            block.Block(
              ir.Call(qualifiedName, Seq(compileExpression(recv), resultVar)),
              resultVar
            )
          } else {
            // TODO: Mutable field read
            ???
          }
        case _ => throw IllegalStateException(s"Cannot compile select from receiver type ${recv.typ}, $recv")
    case superCall@Super(args) =>
      val superClassName = superCall.target match
        case Some((c: ClassDef, _)) => c.name
        case _ => throw IllegalStateException(s"Unresolved target for super call '$superCall'")
      block.Block(
        ir.Call(superClassName, ir.Var("this") +: args.map(compileExpression)),
        ir.Var(gensym.freshName("_"))
      )
    case constrCall@ConstructorCall(name, _, args) =>
      val (classDef, constrDef) = constrCall.target match
        case Some((c, constr)) => (c, constr)
        case _ => throw IllegalStateException(s"Unresolved target for constructor call '$constrCall'")
      if (classDef.isCaseClass) {
        val sidVar = ir.Var(gensym.fresh("sid"))
        val caseName = s"SID$$${signatureString(constrDef.signature)}"
        val caseArgs = irstring.StringLit(classDef.name) +: args.map(compileExpression)
        block.Block(ir.Eq(sidVar, irdata.Construct(caseName, caseArgs)), sidVar)
      } else {
        val oidVar = ir.Var(gensym.fresh("oid"))
        val caseArgs = Seq(irstring.StringLit(classDef.name))
        val dataConstr = irdata.Construct("OID", caseArgs)
        val allocVar = ir.Var(gensym.freshName(Alloc.name))
        block.Block(Seq(
          irimpure.Impure(allocVar, ir.Eq(oidVar, dataConstr), irarith.Add(allocVar, irarith.IntNum(1)), Alloc),
          ir.Call(name, ir.Var("this") +: args.map(compileExpression)),
        ), oidVar)
      }

    case methodCall@MethodCall(recv, fun, _, args, isFix) =>
      val (classDef, methodDef) = methodCall.target match
        case Some((c, m)) => (c, m)
        case _ => throw IllegalStateException(s"Unresolved target for method call '$methodCall'")
      val qualifiedMethodName = s"${methodDef.name}$$${signatureString(methodDef.signature)}"
      val dispatchName = s"dispatch$$$qualifiedMethodName"
      val srcClsVar = ir.Var(gensym.fresh("C"))
      val trgClsVar = ir.Var(gensym.fresh("D"))
      val recvTerm = compileExpression(recv)
      val resultVar = ir.Var(gensym.fresh("return$"))
      block.Block(Seq(
        // Read the runtime type
        matchRuntimeType(recvTerm, srcClsVar),
        ir.Call(dispatchName, Seq(srcClsVar, trgClsVar)),
        ir.Call(qualifiedMethodName, trgClsVar +: (compileExpression(recv) +: args.map(compileExpression)) :+ resultVar)
      ), resultVar)
    case TypeCast(recv, toTyp) =>
      // TODO: Collect values in Cast relation for cast error
      compileExpression(recv)
    case InstanceOf(recv, t: TName) if t.isBuiltIn =>
      throw IllegalStateException(s"Can not typecast to builtin type $t")
    case InstanceOf(recv, t: TName) =>
      val srcClsVar = ir.Var(gensym.fresh("C"))
      val recvTerm = compileExpression(recv)
      val resultVar = ir.Var(gensym.fresh("isInstanceOf$"))
      block.Block(Seq(
        // Read the runtime type
        matchRuntimeType(recvTerm, srcClsVar),
        disjunction.Disjunction(
          Seq(
            ir.Call(subtypeRelationName, Seq(srcClsVar, irstring.StringLit(t.name))),
            ir.Eq(resultVar, bool.BoolTrue)
          ),
          Seq(
            ir.NegCall(subtypeRelationName, Seq(srcClsVar, irstring.StringLit(t.name))),
            ir.Eq(resultVar, bool.BoolFalse)
          ),
        )
      ), resultVar)
    case InstanceOf(recv, t) =>
      throw IllegalStateException(s"Can not typecast to type $t")
    case Tuple(exps) => irtuple.TupleLit(exps.map(compileExpression))
    case SetExp(exps, tty) => irset.SetLit(exps.map(compileExpression))
    case SetMember(name, recv, predicate) =>
      // TODO: Set member
      ???
    case SetComprehension(member, body) =>
      // TODO: Set Comprehension
      ???

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
