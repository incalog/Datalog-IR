package inca.souffle.frontend.compile

import inca.ir
import inca.souffle.syntax.{Atom, Attribute, BinOp, IntrinsicFunctor, Program, ProgramContent, Term, UnOp}
import inca.souffle.syntax.ProgramContent.{ComponentDecl, RelationDecl}
import inca.souffle.syntax.TypeDeclConstraint.ADTType

case class QName(ns: Seq[String]):
  override def toString: String = ns.mkString("$")

trait RequirementAnalysis:
  var requiredDecls: Map[ComponentDecl, Set[(QName, ProgramContent)]] = Map()
  var providedDecls: Map[ComponentDecl, Set[(QName, ProgramContent)]] = Map()

  // parent -> child relations regarding the nesting sturcture, not component inheritance!
  private var parentChildRelations: Map[ComponentDecl, ComponentDecl] = Map()

  private var currentComponent: Option[ComponentDecl] = None

  private def initRequirement(compDecl: ComponentDecl): Unit =
    requiredDecls += compDecl -> Set()
  private def addRequirement(ns: Seq[String], decl: ProgramContent, compDecl: Option[ComponentDecl]): Unit =
    currentComponent match
      case Some(comp) => requiredDecls += comp -> (requiredDecls(comp) + ((QName(ns), decl)))
      case _ => // nothing

  private def initProvision(compDecl: ComponentDecl): Unit =
    providedDecls += compDecl -> Set()
  private def addProvision(ns: Seq[String], decl: ProgramContent, compDecl: Option[ComponentDecl]): Unit =
    currentComponent match
      case Some(comp) => providedDecls += comp -> (providedDecls(comp) + ((QName(ns), decl)))
      case _ => // nothing

  def analyseProgram(prog: Program): Unit =
    analyseContents(prog.content)

    /*requiredDecls.foreach {
      case (decl, rules) =>
        println(s"Declaration: ${decl.ty.n}")
        rules.map { r =>
          println(s"\t${r._1}")
        }
    }*/

    // propagate information based on inheritance
    var changed = true
    //println(requiredDecls.map(_._1.ty.n))
    while (changed) {
      changed = requiredDecls.exists { (compDecl, compRequired) =>
        val superComps = compDecl.superTys.map(_.target.get)
        val oldReq = requiredDecls
        val inherited = superComps.flatMap(providedDecls).toSet
        requiredDecls += compDecl -> (inherited ++ compRequired)
        oldReq != requiredDecls
      }
    }

    /*println("After inheritance propagation")
    requiredDecls.foreach {
      case (decl, rules) =>
        println(s"Declaration: ${decl.ty.n}")
        rules.map { r =>
          println(s"\t${r._1}")
        }
    }*/

    // propagate information along the nesting structure
    changed = true
    while (changed) {
      changed = parentChildRelations.exists { (parent, child) =>
        val oldReq = requiredDecls
        requiredDecls += parent -> (requiredDecls(parent) ++ requiredDecls(child) -- providedDecls(parent))
        oldReq != requiredDecls
      }
    }

    /*println("After propagation: ")
    requiredDecls.foreach {
      case (decl, rules) =>
        println(s"Declaration: ${decl.ty.n}")
        rules.map { r =>
          println(s"\t${r._1}")
        }
    }*/

  def analyseContents(contents: Seq[ProgramContent]): Unit =
    contents.foreach(analyseContent)

  def analyseContent(content: ProgramContent): Unit = content match
    case comp: ComponentDecl =>
      val oldComponent = currentComponent
      if oldComponent.isDefined then
        parentChildRelations += oldComponent.get -> comp
      currentComponent = Some(comp)
      initRequirement(comp)
      initProvision(comp)
      analyseContents(comp.content)
      currentComponent = oldComponent
    case relDecl: RelationDecl =>
      relDecl.names.foreach { n =>
        addProvision(Seq(n), relDecl, currentComponent)
      }
    case fact: ProgramContent.Fact => //if fact.name.ns.size == 1 =>
      val (relDecl, _) = fact.target.get
      // fact is defined outside the current component
      if relDecl.target != currentComponent then
        addRequirement(fact.name.ns, relDecl, currentComponent)
    case rule: ProgramContent.Rule =>
      rule.heads.foreach {
        case a: Atom.Call =>//if a.qualifiedName.ns.size == 1 =>
          val relDecl = a.target.get
          val compDecl = relDecl.target
          val relName = a.qualifiedName.ns
          if compDecl != currentComponent then
            addRequirement(relName, relDecl, currentComponent)
          else
            // we only care about the body of a rule, if the relation is declared in the current component
            analyseAtom(rule.body)
        case _ => None // nothing
      }
    case typeDecl@ProgramContent.TypeDecl(name, _: ADTType) =>
      addProvision(Seq(name), typeDecl, currentComponent)
    case _ => // nothing

  def analyseAtom(atom: Atom): Unit = atom match
    case Atom.Not(atom) =>
      analyseAtom(atom)
    case call@Atom.Call(qname, args) => //if qname.ns.size == 1 =>
      analyseTerms(args:_*)
      val relDecl = call.target.get
      val compDecl = relDecl.target
      val relName = qname.ns
      if compDecl != currentComponent then
        addRequirement(relName, relDecl, currentComponent)
    //case call@Atom.Call(_, args) =>
    //  analyseTerms(args:_*)
    case Atom.Disjunction(bodys) =>
      bodys.map(_.map(analyseAtom))
    case Atom.Compare(t1, _, t2) =>
      analyseTerms(t1, t2)
    case Atom.Match(t1, t2) =>
      analyseTerms(t1, t2)
    case Atom.Contains(t1, t2) =>
      analyseTerms(t1, t2)
    case _ => // nothing

  def analyseTerms(terms: Term*): Unit =
    terms.foreach(analyseTerm)

  def analyseTerm(term: Term): Unit = term match
    case Term.List(s) => analyseTerms(s:_*)
    // TODO: support qualified names
    case constr@Term.Constr(qname, args) => // if qname.ns.size == 1 =>
      analyseTerms(args:_*)
      val typeDecl = constr.target.get
      val compDecl = typeDecl.target
      val caseName = qname.ns
      if compDecl != currentComponent then
        addRequirement(caseName, typeDecl, currentComponent)
    case constr@Term.Constr(qname, args) =>
      analyseTerms(args:_*)
    case Term.TypeCast(t, _) => analyseTerms(t)
    case Term.IntrinsicFunctorApp(_, args) => analyseTerms(args:_*)
    case Term.UserDefFunctorApp(_, args) => analyseTerms(args:_*)
    case Term.Unary(_, t) => analyseTerms(t)
    case Term.Binary(t1, _, t2) => analyseTerms(t1, t2)
    case _ => // nothing