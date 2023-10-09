package inca.viatra.ir.data

import inca.util.Scala.{AppInfix, Id, Lam, Select, StringLiteral}
import inca.ir.Hint.preserveHints
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Language, ModuleEntry, Name, Param, Relation, Term, TermType, Type, Var, string2name}
import inca.ir.extension.*
import inca.ir.extension.data.*
import inca.ir.extension.disjunction.Disjunction
import inca.ir.lowering.BaseLowering
import inca.util.Scala
import inca.viatra.ir.primitiveScala
import inca.viatra.ir.primitiveScala.{Application, Constant, TScala}

trait ScalaLowering extends BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set(primitiveScala.IR, block.IR, disjunction.IR)

  private var freshCount = 0

  def freshName(): Name =
    val x = s"constructed$$$freshCount"
    freshCount += 1
    Name(x)

  private def relationName(dataName: Name, caseName: Name) = s"${dataName}_${caseName}"

  override def visitModuleEntry(moduleEntry: ModuleEntry): Seq[ModuleEntry] = preserveHints(moduleEntry)(moduleEntry match
    case DataDefinition(dataName, cases) =>
      cases.map { case CaseDefinition(caseName, args) =>
        val dataTy = visitType(TData(dataName))
        val name = relationName(dataName, caseName)
        val columns = args.zipWithIndex.map { case (ty, i) => Param(s"arg$i", visitType(ty)) }
        val outColumn = Param("out", dataTy)

        val argVars = columns.map(p => Var(p.name))
        // TODO: We want to generate some kind of ID here
        /*val combine = columns.fold[Scala.Term](StringLiteral("")) { case (acc, Param(name, ty)) =>
          val toStringApp = Application(
            Var("t$0"),
            Lam(Seq("x" -> Some(Scala.TypeName("Any"))), Select(Id("x"), "toString")),
            Seq(Var(name))
          )
          Application(
            Var("t$1"),
            Lam(Seq("x" -> None), AppInfix(acc, "+", Id("x"))),
            Seq(Var("t$0"))
          )
        }  */

        /*val body = Body(
          Application(Var("tmp$1"), Scala.Ter, argVars)
          Eq()
        )*/
        Relation(name, columns :+ outColumn, Seq())
      }
    case _ => super.visitModuleEntry(moduleEntry))

  override def visitTerm(term: Term): Seq[Term] = preserveHints(term)(term match
    case Construct(name, data) =>
      val dataName = term.typ match
        case Some(TermType(TData(name),_)) => name
        case Some(TermType(ty,_)) => throw new IllegalStateException(s"Expected TData, but got $ty")
        case None => throw new IllegalStateException(s"Untyped term $term")
      val relName = relationName(dataName, name)
      val outName = Var(freshName())
      Seq(block.Block(
        // TODO: We want to generate new data here. That is, insert demand here
        Seq(Call(relName, data.flatMap(visitTerm) :+ outName)),
        outName
      ))
    case _ => super.visitTerm(term))

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case Deconstruct(t, caseName, patVars) =>
      // TODO
      Seq(atom)
    case _ => super.visitAtom(atom))

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TData(name) => TScala(Scala.TypeName("String"))
    case _ => super.visitType(ty))