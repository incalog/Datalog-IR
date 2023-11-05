package inca.frontend.oodl.compile

import inca.frontend.oodl.syntax.*
import inca.ir
import inca.ir.{ExtensionalRelation, Language, Name, string2name, name2string}
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

class GenerateIR:
  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + bool.IR + irdata.IR + irmatch.IR
    + demand.IR + disjunction.IR + irnot.IR + irset.IR + irstring.IR + irtuple.IR
    + iragg.IR + iraggset.IR + irimpure.IR
  )

  val gensym: Gensym = new Gensym()

  def compileModule(m: Module): ir.Module = ???

  /** Module content */
  def compileMainFunction(f: FunctionDef): ir.Relation = ???

  def compileClassHierachy(classes: Seq[ClassDef]): ir.Relation = ???

  def compileClassDef(f: FunctionDef): Seq[ir.Relation] = ???

  /** Class content */

  def compileDispatchTable(classes: Seq[ClassDef]): ir.Relation = ???

  def compileMethodDef(m: MethodDef): ir.Relation = ???

  def compileConstructorDef(c: ConstructorDef): ir.Relation = ???

  def compileFieldDef(f: FieldDef): ir.Relation = ???

  /** Statement */

  def compileStatements(stmts: Seq[Statement], resultVar: Name): Seq[ir.Atom] = stmts match
    case Nil => Seq()
    case (stm@Return(_)) :: _ => Seq(compileStatement(stm, resultVar))
    case (stm@If(cnd, thn, els)) :: rest =>
      // get all VarPhiAssigns and merge them in the thn and els branch
      // Note: This assumes, that all VarPhiAssigns directly follow an if stmt
      val (varPhiAssigns, remainingStmts) = rest.span {
        case VarPhiAssign(name, typ, ifStmt, _, _) => stm == ifStmt
        case _ => false
      }
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

  def compileExpression(expr: Expression): ir.Term = expr match
    case NullLit() => ???

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
    case Select(recv, targetName) =>
      recv.typ match
        case Some(t: TTuple) => ??? // Project by parsing targetName
        case Some(t: TName) => ??? // FieldRead
        case _ => throw IllegalStateException(s"Cannot compile select from receiver type ${recv.typ}, $recv")
    case Super(args) => ???
    case ConstructorCall(name, tyArgs, args) => ???
    case MethodCall(recv, fun, tyArgs, args, isFix) => ???
    case TypeCast(recv, toTyp) => ???
    case InstanceOf(recv, ofTyp) => ???
    case Tuple(exps) => ???
    case SetExp(exps, tty) => ???
    case SetMember(name, recv, predicate) => ???
    case SetComprehension(member, body) => ???



