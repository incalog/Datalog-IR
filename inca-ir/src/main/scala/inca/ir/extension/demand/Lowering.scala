package inca.ir.extension.demand

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.lowering.BaseLowering
import inca.ir.util.Gensym
import inca.ir.visitors.VarCollector
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Name, Param, Relation, Term, Var}

import scala.collection.mutable.ListBuffer

object Lowering:
  def apply[S <: IR, T <: BaseIR](srcIR: S, trgIR: T): Lowering[S, T] = new Lowering[S, T] {
    override def src: S = srcIR
    override def trg: T = trgIR
  }

trait Lowering[S <: IR, T <: BaseIR] extends BaseLowering[S, T] with AdornmentAnalysis:
  override def loweredIRs: Set[BaseIR] = super.loweredIRs ++ Set(IR)

  enum Mode:
    case AdornmentAnalysis
    case InsertDemandGuards
    case DeriveDemandRules
  private var mode: Mode = Mode.AdornmentAnalysis

  private var demandRules: Map[Name, ListBuffer[(Seq[Atom], Seq[Term])]] = Map()
  private def addDemandRule(rel: Name, prefix: Seq[Atom], inputArgs: Seq[Term]): Unit =
    demandRules(rel) += ((prefix, inputArgs))
  private def demandRelations: Seq[Relation] =
    for ((rel, ruleBuf) <- demandRules.toSeq) yield {
      val rules = ruleBuf.toList

      val vars = new ListBuffer[String]()
      for ((prefix, inputArgs) <- rules) {
        prefix.foreach(p => vars ++= p.vars.map(_.name.name))
        inputArgs.foreach(a => vars ++= a.vars.map(_.name.name))
      }
      val gensym = new Gensym(vars)
      val params = demandParamsOf(rel).map(p => Param(gensym.freshName(p.name), p.ty))

      val bodies = for ((prefix, inputArgs) <- rules) yield {
        val eqs = params.zip(inputArgs).map { case (Param(pname, _), arg) =>
          Eq(Var(pname), arg)
        }
        Body(prefix ++ eqs)
      }
      Relation(demandRelationName(rel), params, bodies)
    }

  override def visit(module: ir.Module): ir.Module = {
    mode = Mode.AdornmentAnalysis
    val m1 = super.analyzeModule(module)
    mode = Mode.InsertDemandGuards
    val m2 = super.visit(m1)
    mode = Mode.DeriveDemandRules
    val m3 = super.visit(m2)
    val demandRels = demandRelations
    m3.copy(contents = m3.contents ++ demandRels)
  }

  private val bodyPrefix: ListBuffer[Atom] = ListBuffer()
  override def visitBody(body: Body): Seq[Body] =
    bodyPrefix.clear()
    Seq(Body(body.atoms.flatMap { a =>
      val as = visitAtom(a)
      bodyPrefix += a
      as
    }))

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    mode match
      case Mode.AdornmentAnalysis => super.visitAtom(atom)
      case Mode.InsertDemandGuards => atom match
        case Demand(ts) =>
          // replace demand atom by call to input relation
          demandRules += currentRelation.name -> ListBuffer()
          Seq(Call(demandRelationName(currentRelation.name), ts.flatMap(visitTerm)))
        case Call(name, args) =>
          val demandArgs = demandParamsOf(currentRelation.name).map(p => Var(p.name))
          if (demandArgs.isEmpty)
            super.visitAtom(atom)
          else {
            demandRules += currentRelation.name -> ListBuffer()
            Call(demandRelationName(currentRelation.name), demandArgs)
              +: super.visitAtom(atom)
          }
        case _ => super.visitAtom(atom)
      case Mode.DeriveDemandRules => atom match
        case Call(rel, args) if demandedParams.get(rel.name).nonEmpty =>
          val demandedArgs = demandedArgsOf(rel, args)
          addDemandRule(rel, bodyPrefix.toList, demandedArgs)
          super.visitAtom(atom)
        case _ => super.visitAtom(atom)
  }

