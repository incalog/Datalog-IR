package inca.ir.extension.data

import inca.Scala
import inca.Scala.{App, AppInfix, Id, Lam, Select, StringLiteral}
import inca.ir.Hint.preserveHints
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Language, ModuleEntry, Name, Param, Relation, Term, TermType, Type, Var, string2name}
import inca.ir.extension.{data, *}
import inca.ir.extension.data.IR
import inca.ir.extension.disjunction.Disjunction
import inca.ir.extension.primitiveScala.{Application, Constant, TScala, IR as ScalaIR}
import inca.ir.lowering.BaseLowering


object ScalaLowering:
  def apply[S <: IR, T <: BaseIR with ScalaIR with block.IR with disjunction.IR](srcIR: S, trgIR: T): ScalaLowering[S, T] = new ScalaLowering[S, T] {
    override def src: S = srcIR
    override def trg: T = trgIR
  }

trait ScalaLowering[S <: IR, T <: BaseIR with ScalaIR with block.IR with disjunction.IR] extends BaseLowering[S, T]:
  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(IR)

  override def addedIRs: Set[BaseIR] = super.addedIRs ++ Set(ScalaIR)

  private var freshCount = 0

  def freshName(): Name =
    val x = IR.name + "$" + freshCount
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
        // TODO: We want to generate new data here. That is, insert a prefix or demand
        //  placeholder.
        Seq(Call(relName, data.flatMap(visitTerm) :+ outName)),
        outName
      ))
    case _ => super.visitTerm(term))

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom)(atom match
    case Match(matchee, cases) =>
      val tys = matchee.typ match
        case Some(TermType(ty, _)) => ty.flatten
        case None => throw new IllegalStateException(s"Untyped term $matchee")

      visitTerm(matchee).zip(tys).map {
        case (d, TData(dataName)) =>
          val alternatives = cases.map { case Case(name, vars, body) =>
            // TODO: We only want to query here, not generate new Data
            Call(relationName(dataName, name), vars.flatMap(visitTerm) :+ d) +: body
          }
          Disjunction(alternatives)
        case (_, ty) => throw new IllegalStateException(s"Expected TData, but got $ty")
      }
    case _ => super.visitAtom(atom))

  override def visitType(ty: Type): Type = preserveHints(ty)(ty match
    case TData(name) => TScala(Scala.TypeName("String"))
    case _ => super.visitType(ty))