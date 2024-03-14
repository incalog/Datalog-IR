package inca.bddbddb.frontend.compile

import inca.bddbddb.syntax.*
import inca.bddbddb.syntax.ProgramContent.{DomainDecl, RelationDecl, Rule}
import inca.ir
import inca.ir.{Language, Name}
import inca.ir.extension.arithmetic as irarith
import inca.util.FileUtil

object GenerateIR:
  val defaultDomainName: ir.Name = ir.Name("_$default")
  def inputName(name: ir.Name): ir.Name = ir.Name(s"input_$name")
  def domainInputName(name: ir.Name): ir.Name = ir.Name(s"input_domain_$name")

import GenerateIR.defaultDomainName

class GenerateIR:
  private val bddbddbDir = "inca/bddbddb/"

  type DomainMapping = Map[String, Long]

  trait Context:
    val rules: Map[ProgramContent.RelationDecl, Seq[ProgramContent.Rule]] = Map()
    // domainName -> (size, Name -> Int)
    val domains: Map[ir.Name, (Long, DomainMapping)] = Map()

  val irLang: Language = new Language(Set(ir.BaseIR) + irarith.IR)

  private def loadPaFile(baseDir: Option[String], fileName: String): Seq[ProgramContent.DomainDecl] =
    val path = bddbddbDir + (baseDir match
      case Some(dir) => s"$dir/$fileName"
      case None => fileName)
    val content = FileUtil.readFileFromResource(path)
    Parser.parseModule(content).content.collect {
      case decl: ProgramContent.DomainDecl => decl
    }

  private def loadDeclMapping(baseDir: Option[String], fileName: String): DomainMapping =
    val path = bddbddbDir + (baseDir match
      case Some(dir) => s"$dir/$fileName"
      case None => fileName)
    val lines = FileUtil.readLinesFromResource(path)
    lines.zipWithIndex.map {
      case (name, idx) => name -> idx.toLong
    }.toMap

  private def collectRules(name: ir.Name, contents: Seq[ProgramContent]): Seq[ProgramContent.Rule] =
    contents.collect {
      case rule@ProgramContent.Rule(`name`, _, _, _) => rule
    }

  // We are allowed to define a Relation declaration multiple times, with different options
  // This methods collapses these into a single declaration
  private def collapseDuplicateRelationDefinitions(content: Seq[ProgramContent]): Seq[ProgramContent] =
    var relDecls: Map[Name, (Seq[Attribute], Seq[RelationOption])] = Map()
    var remainingContent: Seq[ProgramContent] = Seq()
    content.foreach {
      case decl@RelationDecl(name, attrs, options) =>
        relDecls.get(name) match
          case Some((_, existing)) =>
            relDecls += name -> (attrs, existing ++ options)
          case None =>
            relDecls += name -> (attrs, options)
            decl
      case cont =>
        remainingContent ++= Seq(cont)
    }

    val collapsedRelDecls = relDecls.map {
      case (name, (attrs, options)) => RelationDecl(name, attrs, options)
    }
    remainingContent ++ collapsedRelDecls

  // There are rules without a corresponding relation declaration, we add these back in
  private def insertMissingRelationDecl(content: Seq[ProgramContent]): Seq[ProgramContent] =
    val defaultDomainDecl = ProgramContent.DomainDecl(defaultDomainName, 0, None)
    val relNames = content.collect {
      case ProgramContent.RelationDecl(name, _, _) => name
    }.toSet
    val rulesWithMissingRelDecls = content.collect {
      case r@ProgramContent.Rule(name, _, _, _) if !relNames.contains(name) => r
    }.groupBy(_.name)
    val missingRelDecls = rulesWithMissingRelDecls.map { (name, rules) =>
      val repRule = rules.head
      val attrs = repRule.params.zipWithIndex.map { (_, i) =>
        Attribute(Name(s"param$$$i"), Domain(defaultDomainName))
      }
      ProgramContent.RelationDecl(repRule.name, attrs, Seq())
    }.toSeq
    missingRelDecls ++ content

  def compileProgram(prog: Program, name: String): ir.Module =
    // Load the base dir
    val baseDir = prog.content.collectFirst {
      case ProgramContent.Directive(DirectiveQualifier.BaseDir(dir)) => dir
    }
    // Load all Domain decls from external files
    val extDomains = prog.content.flatMap {
      case ProgramContent.Directive(DirectiveQualifier.Include(fileName)) => loadPaFile(baseDir, fileName)
      case _ => Seq()
    }

    // Sometimes the examples mess up and have duplicate relation decls
    var newProgContent = prog.content ++ extDomains
    newProgContent = collapseDuplicateRelationDefinitions(newProgContent)
    newProgContent = insertMissingRelationDecl(newProgContent)
    val newProg = Program(newProgContent)

    // Infer domain information for each term
    val domainChecker = new DomainInference
    domainChecker.inferProgram(newProg)

    // create a new context object
    val ctx = new Context {
      override val rules = newProgContent.flatMap {
        case decl@ProgramContent.RelationDecl(name, _, _) => Some(decl -> collectRules(name, prog.content))
        case _ => None
      }.toMap
      override val domains = newProgContent.flatMap {
        case DomainDecl(name, size, Some(qn)) => Some(name -> (size, loadDeclMapping(baseDir, qn.toString)))
        case DomainDecl(name, size, None) => Some(name -> (size, Map()))
        case _ => None
      }.toMap
    }
    given Context = ctx

    ir.Module(ir.Name(name), irLang, compileProgramContents(newProgContent))

  def compileProgramContents(content: Seq[ProgramContent])(implicit ctx: Context): Seq[ir.ModuleEntry] =
    content.flatMap(c => compileProgramContent(c)(ctx))

  // If we have a file name for the domain, then we use TString, other TInt
  private def compileDomainType(dom: Domain)(implicit ctx: Context): ir.Type = irarith.TInt

  private def compileAttributes(attrs: Seq[Attribute])(implicit ctx: Context): Seq[ir.Param] =
    attrs.zipWithIndex.map { (attr, idx) =>
      ir.Param(Name(attr.name.name + s"$$$idx"), compileDomainType(attr.domain))
    }

  def compileProgramContent(content: ProgramContent)(implicit ctx: Context): Seq[ir.ModuleEntry] = content match
    case decl@ProgramContent.RelationDecl(name, params, options) => compileRelationDecl(decl)
    case ProgramContent.Rule(name, param, body, options) => Seq() // nothing
    case decl@ProgramContent.DomainDecl(name, size, qualifiedFileName) => compileDomainDecl(decl)
    case ProgramContent.Directive(dirQualifier) => Seq() // nothing

  private def compileDomainDecl(decl: ProgramContent.DomainDecl): Seq[ir.ModuleEntry] =
    Seq(ir.ExtensionalRelation(GenerateIR.domainInputName(decl.name), Seq(ir.Param(ir.Name("d"), irarith.TInt))))

  private def isInputDelc(decl: ProgramContent.RelationDecl): Boolean =
    decl.options.contains(RelationOption.Input) || decl.options.contains(RelationOption.InputTuples)

  def compileRelationDecl(decl: ProgramContent.RelationDecl)(implicit ctx: Context): Seq[ir.ModuleEntry] =
    val params = compileAttributes(decl.attrs)

    var edbBody: Option[ir.Body] = None
    var edbRel: Option[ir.ExtensionalRelation] = None

    // Load external inputs for this relation
    if (isInputDelc(decl)) {
      edbBody = Some(
        ir.Body(Seq(
          ir.ExtensionalCall(GenerateIR.inputName(decl.name), params.map(param => ir.Var(param.name).arg))
        ))
      )
      edbRel = Some(
        ir.ExtensionalRelation(GenerateIR.inputName(decl.name), params)
      )
    }
    val rules = ctx.rules(decl)
    val rel = ir.Relation(
      decl.name,
      params,
      rules.map(r => compileRule(decl, r)) ++ edbBody
    )
    Seq(rel) ++ edbRel
  
  private def compileRule(decl: ProgramContent.RelationDecl, rule: ProgramContent.Rule)(implicit ctx: Context): ir.Body =
    val relName = decl.name
    val params = compileAttributes(decl.attrs)
    val domainInputs = decl.attrs.zip(params).flatMap { (a, p) =>
      val dom = a.domain
      if (dom.canonicalName == defaultDomainName) {
        None
      } else {
        val cName = GenerateIR.domainInputName(dom.canonicalName)
        Some(ir.ExtensionalCall(cName, Seq(ir.Var(p.name).arg)))
      }
    }
    val renameConstraints = params.zip(rule.params).map {
      case (param, term) => ir.Eq(ir.Var(param.name), compileTerm(term))
    }
    ir.Body(
      domainInputs ++ rule.body.map(compileAtom) ++ renameConstraints,
    )

  private def compileAtom(atom: Atom)(implicit ctx: Context): ir.Atom = atom match
    case Atom.Call(rel, args, neg) => ir.Call(rel, args.map(compileArg), neg)
    case Atom.Compare(lhs, "=", rhs) => ir.Eq(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, "!=", rhs) => ir.Eq(compileTerm(lhs), compileTerm(rhs), true)
    case Atom.Compare(lhs, "<", rhs) => irarith.LT(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, ">", rhs) => irarith.GT(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, "<=", rhs) => irarith.LE(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, ">=", rhs) => irarith.GE(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, "=>", rhs) => ???

  private def compileArg(term: Term)(implicit ctx: Context): ir.Arg = term match
    case Term.Var(ir.Name("_")) => ir.WildcardArg()
    case _ => compileTerm(term).arg

  private def compileTerm(term: Term)(implicit ctx: Context): ir.Term = term match
    case Term.Var(name) => ir.Var(name)
    case Term.NumberLit(value) => irarith.IntNum(value)
    case Term.StringLit(value) =>
      val dom = term.target match
        case Some(d) => d
        case None => throw IllegalStateException(s"No domain for string literal: $term")
      val (size, mapping) = ctx.domains(dom.canonicalName)
      val idx = mapping(value)
      irarith.IntNum(idx.toInt)