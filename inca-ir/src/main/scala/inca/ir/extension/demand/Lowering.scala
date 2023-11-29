package inca.ir.extension.demand

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.{Aggregate, AggregateArg}
import inca.ir.lowering.BaseLowering
import inca.ir.{Atom, BaseIR, Body, Call, Eq, Name, Param, RefByName, Relation, Term, Type, Var}
import inca.util.Gensym

import scala.collection.mutable.ListBuffer

trait Lowering extends BaseLowering:
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set()

  enum Phase:
    case InsertDemandGuards
    case DeriveDemandRules
  private var phase: Phase = _

  private var demandRules: Map[Name, ListBuffer[(Seq[Atom], Seq[Term])]] = Map()
  private def addDemandRule(rel: Name, prefix: Seq[Atom], inputArgs: Seq[Term]): Unit =
    demandRules(rel) += ((prefix, inputArgs))
  private def deriveDemandRelations(): Seq[Relation] =
    for ((rel, ruleBuf) <- demandRules.toSeq) yield {
      val rules = ruleBuf.toList

      val vars = new ListBuffer[String]()
      for ((prefix, inputArgs) <- rules) {
        prefix.foreach(p => vars ++= p.vars.map(_.name.name))
        inputArgs.foreach(a => vars ++= a.vars.map(_.name.name))
      }
      val gensym = new Gensym(vars)
      val params = currentModule.relations(rel.name).params.flatMap {
        case Param(name,TDemand(ty)) => Some(Param(gensym.freshName(name),ty))
        case _ => None
      }

      val bodies = for ((prefix, inputArgs) <- rules) yield {
        val eqs = params.zip(inputArgs).map { case (Param(pname, _), arg) =>
          Eq(Var(pname), arg)
        }
        Body(prefix ++ eqs)
      }
      Relation(demandRelationName(rel), params, bodies)
    }

  private var currentModule: ir.Module = _

  protected override def visitModule(module: ir.Module): ir.Module = {
    currentModule = module
    phase = Phase.InsertDemandGuards
    val m1 = super.visitModule(module)
    phase = Phase.DeriveDemandRules
    val m2 = super.visitModule(m1)
    val demandRels = deriveDemandRelations()
    m2.copy(contents = m2.contents ++ demandRels)
  }

  override def visitRelation(rel: Relation): Seq[Relation] = phase match
    case Phase.InsertDemandGuards =>
      val demanded = rel.params.flatMap {
        case Param(name, TDemand(ty)) => Some(Param(name, ty))
        case _ => None
      }
      if (demanded.isEmpty)
        super.visitRelation(rel)
      else
        for (vrel <- super.visitRelation(rel)) yield {
          demandRules += vrel.name -> ListBuffer()
          val guardedBodies = vrel.bodies.map(b => Body(
            Call(demandRelationName(vrel.name), demanded.map(p => Var(p.name)))
              +: b.atoms))
          vrel.copy(bodies = guardedBodies)
        }
    case _ => super.visitRelation(rel)

  override def visitType(ty: Type): Type = ty match
    case TDemand(tty) => preserveHints(ty)(visitType(tty))
    case _ => super.visitType(ty)

  private val bodyPrefix: ListBuffer[Atom] = ListBuffer()
  override def visitBody(body: Body): Seq[Body] =
    bodyPrefix.clear()
    Seq(Body(body.atoms.flatMap { a =>
      val as = visitAtom(a)
      bodyPrefix += a
      as
    }))

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    phase match
      case Phase.DeriveDemandRules => atom match
        case Call(RefByName(rel), args) =>
          val params = currentModule.relations.get(rel.name) match
            case None => Seq()
            case Some(r) => r.params
          val demandedArgs = params.zip(args).flatMap {
            case (Param(_, TDemand(_)), arg) => Some(arg)
            case _ => None
          }
          if (demandedArgs.nonEmpty && !atom.hasHint(Hints.IgnoreCallKey))
            addDemandRule(rel, bodyPrefix.toList, demandedArgs)
          super.visitAtom(atom)
        case Aggregate(rel, args, op) =>
          val params = currentModule.relations.get(rel.name) match
            case None => Seq()
            case Some(r) => r.params
          val demandedArgs = params.zip(args).flatMap {
            case (Param(_, TDemand(_)), arg) =>
              arg match
                case AggregateArg.Arg(tm) =>
                  Some(tm)
                case AggregateArg.AggregateColumn(tm) => Some(tm)
                case _ => None
            case _ => None
          }
          if (demandedArgs.nonEmpty && !atom.hasHint(Hints.IgnoreCallKey))
            addDemandRule(rel, bodyPrefix.toList, demandedArgs)
          super.visitAtom(atom)
        case _ => super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }

