package inca.souffle.frontend.compile

import inca.ir
import inca.souffle.syntax.{Atom, Attribute, BinOp, IntrinsicFunctor, Program, ProgramContent, Term, UnOp}
import inca.souffle.syntax.ProgramContent.{ComponentDecl, RelationDecl}
import inca.souffle.syntax.TypeDeclConstraint.ADTType

trait RequirementAnalysis:
  var requiredDecls: Map[ComponentDecl, Set[(String, ProgramContent)]] = Map()
  var providedDecls: Map[ComponentDecl, Set[(String, ProgramContent)]] = Map()
  private var parentChildRelations: Map[ComponentDecl, ComponentDecl] = Map()

  private var currentComponent: Option[ComponentDecl] = None

  private def initRequirement(compDecl: ComponentDecl): Unit =
    requiredDecls += compDecl -> Set()
  private def addRequirement(name: String, decl: ProgramContent, compDecl: Option[ComponentDecl]): Unit =
    currentComponent match
      case Some(comp) => requiredDecls += comp -> (requiredDecls(comp) + ((name, decl)))
      case _ => // nothing

  private def initProvision(compDecl: ComponentDecl): Unit =
    providedDecls += compDecl -> Set()
  private def addProvision(name: String, decl: ProgramContent, compDecl: Option[ComponentDecl]): Unit =
    currentComponent match
      case Some(comp) => providedDecls += comp -> (providedDecls(comp) + ((name, decl)))
      case _ => // nothing

  def analyseProgram(prog: Program): Unit =
    analyseContents(prog.content)

    // Everything a child requires is required by the parent
    var changed = true
    while (changed) {
      changed = parentChildRelations.exists { (parent, child) =>
        val oldReq = requiredDecls
        requiredDecls += parent -> (requiredDecls(parent) ++ requiredDecls(child) -- providedDecls(parent))
        oldReq != requiredDecls
      }
    }

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
        addProvision(n, relDecl, currentComponent)
      }
    case fact: ProgramContent.Fact if fact.name.ns.size == 1 =>
      val (relDecl, _) = fact.target.get
      // fact is defined outside the current component
      if relDecl.target != currentComponent then
        addRequirement(fact.name.ns.last, relDecl, currentComponent)
    case rule: ProgramContent.Rule =>
      rule.heads.foreach {
        case a: Atom.Call if a.qualifiedName.ns.size == 1 =>
          val relDecl = a.target.get
          val compDecl = relDecl.target
          val relName = a.qualifiedName.ns.last
          if compDecl != currentComponent then
            addRequirement(relName, relDecl, currentComponent)
          else
            // we only care about the body of a rule, if the relation is declared in the current component
            analyseAtom(rule.body)
        case _ => None // nothing
      }
    case typeDecl@ProgramContent.TypeDecl(name, _: ADTType) =>
      addProvision(name, typeDecl, currentComponent)
    case _ => // nothing

  def analyseAtom(atom: Atom): Unit = atom match
    case Atom.Not(atom) =>
      analyseAtom(atom)
    case call@Atom.Call(qname, args) if qname.ns.size == 1 =>
      analyseTerms(args:_*)
      val relDecl = call.target.get
      val compDecl = relDecl.target
      val relName = qname.unqualifiedName
      if compDecl != currentComponent then
        addRequirement(relName, relDecl, currentComponent)
    case call@Atom.Call(_, args) =>
      analyseTerms(args:_*)
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
    case constr@Term.Constr(qname, args) if qname.ns.size == 1 =>
      analyseTerms(args:_*)
      val typeDecl = constr.target.get
      val compDecl = typeDecl.target
      val caseName = qname.unqualifiedName
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