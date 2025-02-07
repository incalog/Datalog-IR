package inca.ir.optimize

import inca.ir.optimize.DisjointRuleAnalysis.{DisjointResult, SmtAtom, SmtTerm}
import inca.ir.typing.Typeable
import inca.ir.*
import inca.ir.analysis.{AnalysisKey, AnalysisResult}
import inca.ir.extension.aggregate.AggregateColumnArg
import inca.ir.extension.arithmetic.{BinCompare, BinOp, IntNum, TInt}
import inca.ir.visitors.IRVisitor
import inca.util.Gensym
import smtlib.interpreters.Z3Interpreter
import smtlib.trees.{Commands, Terms}
import smtlib.trees.CommandsResponses.*

object DisjointRuleAnalysis:
  case class SmtAtom(atom: Terms.Term) extends Atom:
    override def vars: Seq[Var] = Seq()
  case class SmtTerm(term: Terms.Term) extends Term:
    override def vars: Seq[Var] = Seq()

  case object DisjointKey extends AnalysisKey:
    override val key: String = "DisjointRules"
    override type Result = DisjointResult.type
  case object DisjointResult extends AnalysisResult:
    override val akey: AnalysisKey = DisjointKey

trait DisjointRuleAnalysis extends IRVisitor, Optimizer:
  override def name: String = "DisjointRuleAnalysis"

  private val gensym = new Gensym()
  private var varMapping = Map[Var.Target, String]()
  
  var hasAnalyzed = false

  override def analyzeProgram(modules: Seq[Module]): Unit = visitProgram(modules)

  override def visitProgram(modules: Seq[Module], dependencies: Seq[Module]): Seq[Module] = 
    if (!hasAnalyzed)
      hasAnalyzed = true
      super.visitProgram(modules, dependencies)
    else
      modules
      
  
  var currentBody = -1
  override def visitRelation(relation: Relation): Seq[Relation] = gensym.scoped {
    currentBody = -1
    val smtbodies = relation.bodies.flatMap { b =>
      currentBody += 1
      visitBody(b).map(_.atoms.asInstanceOf[Seq[SmtAtom]])
    }
    val smtRules = smtbodies.map(atoms => Terms.FunctionApplication(qident("and"), atoms.map(_.atom)))
    val smtRulePairs =
      for ((r1,i) <- smtRules.zipWithIndex; r2 <- smtRules.drop(i + 1))
        yield Terms.FunctionApplication(qident("and"), Seq(r1, r2))
    val hasOverlapFormula = Terms.FunctionApplication(qident("or"), smtRulePairs)

    val vars = relation.bodies.flatMap(_.vars).toSet
    val smtVarDeclarations = vars.map(v => Commands.DeclareConst(Terms.SSymbol(v.name.name), sortType(v.typ.get.ty)))

//    val smtChallenge =
//      s"""${smtVarDeclarations.mkString("")}
//         |(assert $hasOverlapFormula)
//         |(check-sat)
//         |""".stripMargin
//    println(smtChallenge)

    val z3 = Z3Interpreter.buildDefault
    smtVarDeclarations.foreach(z3.eval)
    z3.eval(Commands.Assert(hasOverlapFormula))
    val response = z3.eval(Commands.CheckSat())
    response match
      case CheckSatStatus(UnsatStatus) =>
        logOptimizationStat("disjoint relation", 1, _+1)
        relation.storeAnalysisResult(DisjointResult)
      case _ => // nothing

    Seq(relation)
  }

  override def visitAtom(atom: Atom): Seq[SmtAtom] = atom match
    case Eq(lhs, rhs, neg) =>
      val terms = Seq(lhs, rhs).flatMap(visitTerm)
      val op = if (!neg) qident("=") else qident("distinct")
      Seq(SmtAtom(Terms.FunctionApplication(op, terms.map(_.term))))
    case Call(ref, args, neg) =>
      val terms = args.flatMap {
        case TermArg(t) => visitTerm(t)
        case WildcardArg() => Seq(SmtTerm(qident(gensym.fresh("wildcard"))))
        case AggregateColumnArg(t) => ???
      }
      Seq(SmtAtom(Terms.FunctionApplication(qident(ref.name), terms.map(_.term))))
    case ExtensionalCall(_, _, _) => Seq() // FIXME
    case BinCompare(lhs, rhs, op) =>
      val terms = Seq(lhs, rhs).flatMap(visitTerm)
      Seq(SmtAtom(Terms.FunctionApplication(qident(op), terms.map(_.term))))
    case _ => throw new MatchError(atom)

  override def visitTerm(term: Term): Seq[SmtTerm] = term match
    case v: Var => Seq(SmtTerm(qident(v.name, v)))
    case Cast(t, ty) => visitTerm(t)
    case IntNum(value) => Seq(SmtTerm(Terms.SNumeral(value)))
    case BinOp(lhs, rhs, op) =>
      val terms = Seq(lhs, rhs).flatMap(visitTerm)
      Seq(SmtTerm(Terms.FunctionApplication(qident(op), terms.map(_.term))))
    case _ => throw new MatchError(term)

  def sortType(ty: Type): Terms.Sort = ty match
    case TInt => Terms.Sort(ident("Int"))
    case _ => throw new MatchError(ty)

  private def qident(name: String) =
    Terms.QualifiedIdentifier(ident(name), None)
  private def qident(name: Name) =
    Terms.QualifiedIdentifier(ident(name), None)
  private def qident(name: Name, t: Typeable[TermType]) =
    Terms.QualifiedIdentifier(ident(name), None) // t.typ.map(tt => visitType(tt.ty).sort))

  private def ident(x: String): Terms.Identifier =
    Terms.Identifier(Terms.SSymbol(x.name))
  private def ident(x: Name): Terms.Identifier =
    ident(x.name)

object A extends App:
  import smtlib.parser.Parser
  val p = Parser.fromString(
    s"""
       |(assert (/= 1 2))
       |""".stripMargin)
  val tree = p.parseCommand.asInstanceOf[Commands.Assert]
  println(tree.term)