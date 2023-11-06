package inca.frontend.oodl.compile

import inca.frontend.oodl.syntax.*
import inca.ir.Name
import inca.util.Gensym

class SSA:
  def compileModule(m: Module): Module =
    val gensym: Gensym = new Gensym(Iterable.empty)
    gensym.register(m.usedModuleNames.map(_.name))
    Module(m.name, m.imports, m.content.map(visitModuleContent(_)(gensym)))

  def visitModuleContent(content: ModuleContent)(implicit gensym: Gensym): ModuleContent = content match
    case c: ClassDef => visitClassDef(c)
    case f: FunctionDef => visitFunctionDef(f)

  def visitClassDef(c: ClassDef)(implicit gensym: Gensym): ClassDef =
    gensym.register(c.name.name)
    ClassDef(c.annos, c.vis, c.name, c.tyVars, c.parentCls, c.content.map(visitClassContent))

  def visitClassContent(c: ClassContent)(implicit gensym: Gensym): ClassContent = c match
    case cd: ConstructorDef => visitConstructorDef(cd)
    case fd: FieldDef => visitFieldDef(fd)
    case md: MethodDef => visitMethodDef(md)

  // OldName -> (NewName, Type)
  type Env = Map[Name, (Name, Type)]
  var env: Env = Map()

  def visitConstructorDef(c: ConstructorDef)(implicit gensym: Gensym): ConstructorDef =
    gensym.register(c.params.map(_.name.name))
    gensym.register("this")
    env = Map()
    ConstructorDef(c.annos, c.vis, c.params, visitStatements(c.body))

  def visitMethodDef(m: MethodDef)(implicit gensym: Gensym): MethodDef =
    gensym.register(m.params.map(_.name.name))
    gensym.register("this")
    env = Map()
    MethodDef(m.annos, m.vis, m.name, m.tyVars, m.params, m.outType, visitStatements(m.body))

  def visitFieldDef(f: FieldDef)(implicit gensym: Gensym): FieldDef =
    f.body match
      case Some(expr) => FieldDef(f.annos, f.vis, f.name, f.typ, Some(visitExpression(expr)), f.immutable)
      case _ => f

  def visitFunctionDef(f: FunctionDef)(implicit gensym: Gensym): FunctionDef =
    gensym.register(f.name.name)
    FunctionDef(f.annos, f.vis, f.name, f.tyVars, f.params, f.outType, visitStatements(f.body))

  def visitStatements(s: Seq[Statement])(implicit gensym: Gensym): Seq[Statement] =
    s.flatMap(visitStatement)

  def visitStatement(s: Statement)(implicit gensym: Gensym): Seq[Statement] = s match
    case Expr(expression) =>
      Seq(Expr(visitExpression(expression)))

    case Return(expression) =>
      Seq(Return(visitExpression(expression)))

    case Super(args) =>
      Seq(Super(args.map(visitExpression)))

    case Assign(v@Var(targetName), rhs) =>
      v.target match
        case Some(varDecl: VarDeclare) =>
          val rhsExp = visitExpression(rhs)
          val typ = varDecl.typ.getOrElse(rhsExp.typ.getOrElse(
            throw new IllegalStateException(s"Untyped variable declaration $s")
          ))
          val newName = Name(gensym.fresh(targetName.name))
          env = env + (targetName -> (newName, typ))
          Seq(
            VarDeclare(newName, Some(typ), Some(rhsExp), true)
          )
        case trg =>
          throw new IllegalStateException(s"Unexpected variable target $trg")

    case Assign(lhs, rhs) =>
      Seq(Assign(visitExpression(lhs), visitExpression(rhs)))

    case VarDeclare(name, typ, maybeExpression, immutable) =>
      gensym.register(name.name)
      val ty = typ.getOrElse(maybeExpression.flatMap(_.typ).getOrElse(
        throw new IllegalStateException(s"Untyped variable declaration $s")
      ))
      val newExpr = maybeExpression.map(visitExpression)
      env = env + (name -> (name, ty))
      Seq(VarDeclare(name, typ, newExpr, true))
    case If(cnd, thn, els) =>
      val oldEnv = env
      val thnStmts = visitStatements(thn)
      val thnEnv = env.filter(kv => oldEnv.contains(kv._1) && !oldEnv.get(kv._1).contains(kv._2))
      env = oldEnv
      val elsStmts = visitStatements(els)
      val elsEnv = env.filter(kv => oldEnv.contains(kv._1) && !oldEnv.get(kv._1).contains(kv._2))
      env = oldEnv

      // base name of all new symbols either used in the then or the else block
      val usedSymbols = thnEnv.keySet.union(elsEnv.keySet).toSeq
      val ifStmt = If(visitExpression(cnd), thnStmts, elsStmts)

      ifStmt +: usedSymbols.map { name =>
        val newName = Name(gensym.fresh(name.name))
        val (thnName, thnType) = thnEnv.getOrElse(name, oldEnv(name))
        val (elsName, elsType) = elsEnv.getOrElse(name, oldEnv(name))
        if (thnType != elsType)
          throw new RuntimeException(s"Type mismatch for variable $newName: ${thnType} != ${elsType}")
        env = env + (name -> ((newName, thnType)))
        VarPhiAssign(newName, thnType, ifStmt, thnName, elsName)
      }

  def visitExpression(e: Expression)(implicit gensym: Gensym): Expression = e match
    case Var(name) =>
      // Rewrite all VarReadExpr to use the latest generated name for the variable
      val (newName, _) = env.getOrElse(name, (name, TAny))
      Var(newName)
    case Select(recv, targetName) =>
      Select(visitExpression(recv), targetName)
    case ConstructorCall(name, tyArgs, args) =>
      ConstructorCall(name, tyArgs, args.map(visitExpression))
    case MethodCall(recv, fun, tyArgs, args, isFix) =>
      MethodCall(visitExpression(recv), fun, tyArgs, args.map(visitExpression), isFix)
    case TypeCast(recv, toTyp) =>
      TypeCast(visitExpression(recv), toTyp)
    case InstanceOf(recv, ofTyp) =>
      InstanceOf(visitExpression(recv), ofTyp)
    case Tuple(exps) =>
      Tuple(exps.map(visitExpression))
    case SetExp(exps, tty) =>
      SetExp(exps.map(visitExpression), tty)
    case SetMember(name, recv, predicate) =>
      gensym.register(name.name)
      val recvExpr = visitExpression(recv)
      val ty = e.typ.getOrElse(
        throw new IllegalStateException(s"Untyped variable declaration $name in $e")
      )
      env = env + (name -> (name, ty))
      SetMember(name, recvExpr, predicate.map(visitExpression))
    case SetComprehension(member, body) =>
      val oldEnv = env
      val setCompr = SetComprehension(member.map(visitExpression), visitExpression(body))
      env = oldEnv
      setCompr
    case BinOp(e1, op, e2) =>
      BinOp(visitExpression(e1), op, visitExpression(e2))
    case UnOp(op, e) =>
      UnOp(op, visitExpression(e))
    case _ => e

