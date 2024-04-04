package inca.ir.extension.demand

import inca.ir
import inca.ir.Hint.preserveHints
import inca.ir.extension.aggregate.{Aggregate, AggregateColumnArg}
import inca.ir.lowering.BaseLowering
import inca.ir.*
import inca.ir.typing.Mode.Binding
import inca.util.Gensym

import scala.annotation.tailrec
import scala.collection.immutable.{AbstractSeq, LinearSeq}
import scala.collection.mutable.ListBuffer

trait LoweringWithOutlining extends BaseLowering:
  override val name: String = "Demand"
  override val loweredIRs: Set[BaseIR] = Set(IR)
  override val requiredIRs: Set[BaseIR] = Set()

  enum Phase:
    case InsertDemandGuards
    case DeriveDemandRules
  private var phase: Phase = _

  private var demandPrefix: Map[Name, (Name, Seq[Atom], Seq[Param])] = Map()

  private def addDemandPrefixRule(rel: Name, prefix: Name, params: Seq[Param], ats: Seq[Atom]): Unit =
    demandPrefix += prefix -> (rel, ats, params)

  private def deriveDemandPrefixRelations(): Seq[Relation] =
    for ((prefix, (rel, ats, params)) <- demandPrefix.toSeq) yield {
      Relation(prefix, params, Set(Body(ats)).toSeq)
    }

  private var demandRules: Map[Name, ListBuffer[(Name, Seq[Var], Seq[Term])]] = Map()

  private def addDemandRule(rel: Name, prefix: Name, prefixCallVars: Seq[Var], inputArgs: Seq[Term]): Unit =
    demandRules(rel) += ((prefix, prefixCallVars, inputArgs))

  private def deriveDemandRelations(): Seq[Relation] =
    for ((rel, ruleBuf) <- demandRules.toSeq) yield {
      val rules = ruleBuf.toList

      val vars = new ListBuffer[String]()
      val gensym = new Gensym(vars)
      val params = currentModule.relations(rel.name).params.flatMap {
        case Param(name,TDemand(ty)) => Some(Param(gensym.freshName(name),ty))
        case _ => None
      }

      val bodies = rules.map { (prefix, prefixCallVars, inputArgs) =>
        val prefixCall = Call(prefix, prefixCallVars.map(_.arg))
        val eqs = params.zip(inputArgs).map { case (Param(pname, _), arg) =>
          Eq(Var(pname), arg)
        }
        Body(prefixCall +: eqs)
      }.distinct

      Relation(demandRelationName(rel), params, bodies)
    }

  private var currentModule: ir.Module = _

  override def visitModule(module: ir.Module): ir.Module = {
    currentModule = module
    phase = Phase.InsertDemandGuards
    val m1 = super.visitModule(module)
    phase = Phase.DeriveDemandRules
    val m2 = super.visitModule(m1)
    val demandRels = deriveDemandRelations()
    val demandPrefixRels = deriveDemandPrefixRelations()
    m2.copy(contents = m2.contents ++ demandPrefixRels ++ demandRels)
  }

  private var currentRelation: Relation = _

  override def visitRelation(rel: Relation): Seq[Relation] =
    currentRelation = rel
    phase match
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
            val guardArgs = demanded.map { case Param(name, ty) =>
              val v = Var(name)
              v.typed(TermType(ty, Binding))
              v.arg
            }
            val guardedBodies = vrel.bodies.map { case Body(ats) =>
              Body(Call(demandRelationName(vrel.name), guardArgs) +: ats)
            }
            vrel.copy(bodies = guardedBodies)
          }
      case _ => super.visitRelation(rel)

  override def visitType(ty: Type): Type = ty match
    case TDemand(tty) => preserveHints(ty)(visitType(tty))
    case _ => super.visitType(ty)

  private val currentPrefixAtoms: ListBuffer[Atom] = ListBuffer()

  override def visitBody(body: Body): Seq[Body] =
    currentPrefixAtoms.clear()
    body.atoms.foreach { a =>
      visitAtom(a)
      currentPrefixAtoms += a
    }
    Seq(Body(currentPrefixAtoms.toSeq))

  override def visitAtom(atom: Atom): Seq[Atom] = preserveHints(atom) {
    phase match
      case Phase.DeriveDemandRules => atom match
        case Call(RefByName(rel), args, false) =>
          val params = currentModule.relations.get(rel.name) match
            case None => Seq()
            case Some(r) => r.params
          val demandedArgs = params.zip(args).flatMap {
            case (Param(_, TDemand(_)), TermArg(t)) => Some(t)
            case (Param(_, TDemand(_)), _) => None
            case _ => None
          }
          if (demandedArgs.nonEmpty && !atom.hasHint(DemandIgnoreCallHint))
            val caller = currentRelation.name
            val callee = rel.name
            val prefixName = gensym.freshGlobal(s"${caller}_$callee")

            val relevantVars = currentPrefixAtoms.flatMap(_.vars).distinct.toSeq
            // TODO: This might make problems when we cast variables to different types
            val params = relevantVars.map(v => Param(v.name, v.typ.get.ty))

            addDemandPrefixRule(rel, prefixName, params, currentPrefixAtoms.toSeq)
            addDemandRule(rel, prefixName, relevantVars, demandedArgs)

            val prefixCall = Call(prefixName, relevantVars.map(_.arg))
            currentPrefixAtoms.clear()
            currentPrefixAtoms += prefixCall

          super.visitAtom(atom)

        case Aggregate(RefByName(rel), args, op) =>
          val params = currentModule.relations.get(rel.name) match
            case None => Seq()
            case Some(r) => r.params
          val demandedArgs = params.zip(args).flatMap {
            case (Param(_, TDemand(_)), arg) =>
              arg match
                case TermArg(tm) => Some(tm)
                case AggregateColumnArg(tm) => Some(tm)
                case _ => None
            case _ => None
          }
          if (demandedArgs.nonEmpty && !atom.hasHint(DemandIgnoreCallHint))
            val caller = currentRelation.name
            val callee = rel.name
            val prefixName = gensym.freshName(s"${caller}_$callee")

            val relevantVars = currentPrefixAtoms.flatMap(_.vars).distinct.toSeq
            // TODO: This might make problems when we cast variables to different types
            val params = relevantVars.map(v => Param(v.name, v.typ.get.ty))

            addDemandPrefixRule(rel, prefixName, params, currentPrefixAtoms.toSeq)
            addDemandRule(rel, prefixName, relevantVars, demandedArgs)

            val prefixCall = Call(prefixName, relevantVars.map(_.arg))
            currentPrefixAtoms.clear()
            currentPrefixAtoms += prefixCall

          super.visitAtom(atom)
        case _ => super.visitAtom(atom)
      case _ => super.visitAtom(atom)
  }

  override def visitTerm(term: Term): Seq[Term] =
    // Preserve the type information
    val Seq(t) = super.visitTerm(term)
    t.typed(term.typ.get)
    Seq(t)

