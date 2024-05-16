package inca.souffle.frontend.compile

import inca.souffle.syntax.{Atom, Attribute, Program, ProgramContent}
import inca.souffle.syntax.ProgramContent.{ComponentDecl, RelationDecl}

trait RequirementAnalysis:
  var requiredDecls: Map[ComponentDecl, Set[(String, ProgramContent.RelationDecl)]] = Map()
  var providedDecls: Map[ComponentDecl, Set[(String, ProgramContent.RelationDecl)]] = Map()
  private var parentChildRelations: Map[ComponentDecl, ComponentDecl] = Map()

  private var currentComponent: Option[ComponentDecl] = None

  private def initRequirement(compDecl: ComponentDecl): Unit =
    requiredDecls += compDecl -> Set()
  private def addRequirement(name: String, decl: ProgramContent.RelationDecl, compDecl: Option[ComponentDecl]): Unit =
    currentComponent match
      case Some(comp) => requiredDecls += comp -> (requiredDecls(comp) + ((name, decl)))
      case _ => // nothing

  private def initProvision(compDecl: ComponentDecl): Unit =
    providedDecls += compDecl -> Set()
  private def addProvision(name: String, decl: ProgramContent.RelationDecl, compDecl: Option[ComponentDecl]): Unit =
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
    case _ => // nothing

  def analyseAtom(atom: Atom): Unit = atom match
    case Atom.Not(atom) =>
      analyseAtom(atom)
    case call@Atom.Call(qname, args) if qname.ns.size == 1 =>
      val relDecl = call.target.get
      val compDecl = relDecl.target
      val relName = call.qualifiedName.ns.last
      if compDecl != currentComponent then
        addRequirement(relName, relDecl, currentComponent)
    case Atom.Disjunction(bodys) =>
      bodys.map(_.map(analyseAtom))
    case _ => // nothing