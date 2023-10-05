package inca.ir.extension.arithmetic

import inca.Scala
import inca.ir.Hint.preserveHints
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Language, ModuleEntry, Name, Param, Relation, Term, TermType, Type, Var, string2name}
import inca.ir.extension.block
import inca.ir.extension.primitiveScala
import inca.ir.extension.primitiveScala.{Application, Constant, TScala}

trait ScalaLowering extends BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(primitiveScala.IR, block.IR)

  private var freshCount = 0
  def freshName(): Name =
    val x = IR.name + "$" + freshCount
    freshCount += 1
    Name(x)

  // TODO: How do we handle this best so that it is extensible ?
  private def asScala(ty: Type): Scala.Type = ty match
    //case Some(TAny) => Some(Scala.TypeName("Any"))
    case TInt => Scala.TypeName("Int")
    case TDouble => Scala.TypeName("Double")
    case TScala(ty) => ty
    case _ => throw IllegalStateException(s"No scala conversion for Type $ty")

  private def lamOp(op: String, lhs: Scala.Param, rhs: Scala.Param): Scala.Lam =
    Scala.Lam(Seq(lhs, rhs), Scala.AppInfix(Scala.Id(lhs._1), op, Scala.Id(rhs._1)))

  private def app(op: String, ty: TScala, lhsParam: (Term, Type), rhsParam: (Term, Type)): (Var, Application) = {
    val x = Var(freshName())
    val (lhs, lhsTy) = lhsParam
    val (rhs, rhsTy) = rhsParam
    val calc = lamOp(op, Scala.Param("lhs", asScala(lhsTy)), Scala.Param("rhs", asScala(rhsTy)))
    val appl = Application(x, ty, calc, Seq(lhs, rhs))
    (x, appl)
  }

  private def blockApp(op: String, ty: TScala, lhs: (Term, Type), rhs: (Term, Type)): Term = {
    val (x, appl) = app(op, ty, lhs, rhs)
    block.Block(Seq(appl), x)
  }

  private def typedParams(t: Term): Seq[(Term, Type)] = {
    val ty = t.typ match
      case Some(TermType(ty,_)) => ty.flatten
      case None => throw new IllegalStateException(s"Untyped term $t")

    for ((t, i) <- visitTerm(t).zipWithIndex)
      yield t -> ty(i)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case LT(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) =>
        val (x, appl) = app("<", TScala.bool, l, r)
        // TODO: We get back a scala bool here...
        Eq(Constant(Scala.BoolLiteral(true), TScala.bool), x)
      }
    case GT(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) =>
        val (x, appl) = app(">", TScala.bool, l, r)
        // TODO: We get back a scala bool here...
        Eq(Constant(Scala.BoolLiteral(true), TScala.bool), x)
      }
    case _ =>
      super.visitAtom(atom))

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match {
    case IntNum(i) =>
      Seq(Constant(Scala.IntLiteral(i), TScala.int))
    case DoubleNum(d) =>
      Seq(Constant(Scala.DoubleLiteral(d), TScala.double))
    case Add(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("+", TScala.int, l, r) }
    case Sub(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("-", TScala.int, l, r) }
    case Mul(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("*", TScala.int, l, r) }
    case Div(lhs, rhs) =>
      typedParams(lhs).zip(typedParams(rhs)).map { case (l, r) => blockApp("/", TScala.int, l, r) }
    case _ =>
      super.visitTerm(term)
  })

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TInt => TScala(Scala.TypeName("Int"))
    case TDouble => TScala(Scala.TypeName("Double"))
    case _ => super.visitType(ty))
