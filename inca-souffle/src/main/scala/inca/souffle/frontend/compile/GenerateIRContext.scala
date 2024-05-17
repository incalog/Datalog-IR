package inca.souffle.frontend.compile

import inca.souffle.frontend.compile.nameresolution.NameResolution
import inca.souffle.syntax.{Atom, ComponentType, DirectiveQualifier, DirectiveValue, Program, ProgramContent, QualifiedName}
import inca.souffle.syntax.ProgramContent.ComponentDecl

trait GenerateIRContext {
  private var rules: Map[ProgramContent.RelationDecl, Set[ProgramContent]] = Map()
  // Collect for all RelationDecl if it is an edb relation or not
  private var edbDecls: Map[ProgramContent.RelationDecl, Map[String, DirectiveValue]] = Map()
  // Collect for all RelationDecl if it is an output or not
  private var outputDecls: Set[ProgramContent.RelationDecl] = Set()
  // all path for each declaration
  private var paths: Map[ProgramContent, Seq[ComponentType]] = Map()
  // for each component decl store the name and the actual decl that is required
  private var requiredDecls: Map[ComponentDecl, Set[(String, ProgramContent.RelationDecl)]] = Map()

  private var currentComponent: Option[ComponentDecl] = None

  def initContext(prog: Program): Unit =
    val nameResolution = new NameResolution {}
    nameResolution.resolveProgram(prog)

    currentComponent = None

    rules = collectRules(prog.content)
    edbDecls = collectEdbDecls(prog.content)
    outputDecls = collectOutputDecls(prog.content)
    paths = collectPath(prog.content)

    val reqAna = new RequirementAnalysis {}
    reqAna.analyseProgram(prog)
    requiredDecls = reqAna.requiredDecls

  def switchToMainComponent[A](f: => A): A =
    internalSwitchToComponent(None)(f)

  def switchToComponent[A](componentDecl: ComponentDecl)(f: => A): A =
    internalSwitchToComponent(Some(componentDecl))(f)

  private def internalSwitchToComponent[A](componentDeclOption: Option[ComponentDecl])(f: => A): A =
    val oldComponent = currentComponent
    currentComponent = componentDeclOption
    try {
      val a = f
      a
    } finally {
      currentComponent = oldComponent
    }

  def currentlyInMainComponent: Boolean = currentComponent.isEmpty

  def currentlyInComponent(comp: Option[ComponentDecl]): Boolean = currentComponent == comp

  def relationIsRequiredInComponent(name: String, compDecl: ComponentDecl): Boolean =
    lookupRequiredDeclarations(compDecl).map(_._1).contains(name)
  
  def lookupRequiredDeclarations(compDecl: ComponentDecl): Set[(String, ProgramContent.RelationDecl)] =
    requiredDecls.getOrElse(compDecl, Set())

  def lookupPath(decl: ProgramContent): Seq[ComponentType] =
    paths(decl)

  // name and relDecl, since one relDecl might have multiple rules
  def lookupRules(name: String, relDecl: ProgramContent.RelationDecl): Seq[ProgramContent] =
    def ruleHasName(rule: ProgramContent, relName: String): Boolean = rule match
      // A single rule can have multiple names. Only a single name must match
      case ProgramContent.Rule(heads, _, _) =>
        heads.exists {
          case Atom.Call(QualifiedName(ns), _) if ns.last == relName => true
          case _ => false
        }
      case ProgramContent.Fact(QualifiedName(ns), args) if ns.last == relName => true
      case _ => false

    rules(relDecl).filter(r => ruleHasName(r, name)).toSeq

  def lookupEdbAttributes(relDecl: ProgramContent.RelationDecl): Map[String, DirectiveValue] =
    edbDecls(relDecl)

  def isEdbDeclaration(relDecl: ProgramContent.RelationDecl): Boolean =
    edbDecls.contains(relDecl)

  def isOutputDeclaration(relDecl: ProgramContent.RelationDecl): Boolean =
    outputDecls.contains(relDecl)

  private def combineIterables[K, V](a: Map[K, Set[V]], b: Map[K, Set[V]]): Map[K, Set[V]] =
    a ++ b.map { case (k, v) => k -> (v ++ a.getOrElse(k, Set.empty)) }

  private def collectRules(content: Seq[ProgramContent], collectedComponents: Set[ComponentType] = Set()): Map[ProgramContent.RelationDecl, Set[ProgramContent]] =
    // TODO: We can improve this by visiting components only once. We still need to visit them on init!
    var rules: Map[ProgramContent.RelationDecl, Set[ProgramContent]] = Map()
    content.foreach {
      case rule@ProgramContent.Rule(heads, _, _) =>
        val newRules = heads.map {
          case call: Atom.Call =>
            val relDecl = call.target.get
            val existingRules = rules.getOrElse(relDecl, Set())
            relDecl -> (existingRules + rule)
          case a =>
            throw IllegalStateException(s"Unexpected head atom $a")
        }.toMap
        rules = combineIterables(rules, newRules)
      case fact@ProgramContent.Fact(_, _) =>
        val (relDecl, _) = fact.target.get
        val existingRules = rules.getOrElse(relDecl, Set())
        rules += relDecl -> (existingRules + fact)
      case compInit@ProgramContent.ComponentInit(_, compType) =>
        // we must only collect rules in a component, if there is an init for the component
        val compDecl = compInit.target.get
        val newRules = collectRules(compDecl.content)
        rules = combineIterables(rules, newRules)
      case _ => // nothing
    }
    rules

  private def collectEdbDecls(content: Seq[ProgramContent]): Map[ProgramContent.RelationDecl, Map[String, DirectiveValue]] =
    // TODO: This assumes, we do not allow cases such as
    //  .decl A, B
    //  .input A
    var edbDecls = Map[ProgramContent.RelationDecl, Map[String, DirectiveValue]]()
    content.foreach {
      case input@ProgramContent.Directive(DirectiveQualifier.Input, _, _) =>
        edbDecls += input.target.get -> input.attrs
      case compDecl@ProgramContent.ComponentDecl(_, _, compContent) =>
        edbDecls ++= collectEdbDecls(compContent)
      case _ => // nothing
    }
    edbDecls

  private def collectOutputDecls(content: Seq[ProgramContent]): Set[ProgramContent.RelationDecl] =
    // TODO: This assumes, we do not allow cases such as
    //  .decl A, B
    //  .input A
    var outputDecls = Set[ProgramContent.RelationDecl]()
    content.foreach {
      case output@ProgramContent.Directive(DirectiveQualifier.Output, _, _) =>
        outputDecls += output.target.get
      case compDecl@ProgramContent.ComponentDecl(_, _, compContent) =>
        outputDecls ++= collectOutputDecls(compContent)
      case _ => // nothing
    }
    outputDecls

  private def collectPath(content: Seq[ProgramContent], path: Seq[ComponentType] = Seq()): Map[ProgramContent, Seq[ComponentType]] =
    var declToPath: Map[ProgramContent, Seq[ComponentType]] = Map()
    content.foreach {
      case decl: ProgramContent.RelationDecl =>
        declToPath += decl -> path
      case decl: ProgramContent.TypeDecl =>
        declToPath += decl -> path
      case compDecl@ProgramContent.ComponentDecl(compTy, _, compContent) =>
        declToPath ++= collectPath(compContent, path :+ compTy)
      case _ => // nothing
    }
    declToPath
}
