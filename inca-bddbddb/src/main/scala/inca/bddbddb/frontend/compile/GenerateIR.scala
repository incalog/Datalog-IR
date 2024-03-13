package inca.bddbddb.frontend.compile

import inca.bddbddb.syntax.*
import inca.bddbddb.syntax.ProgramContent.{DomainDecl, RelationDecl}
import inca.ir
import inca.ir.{Language, Name}
import inca.ir.extension.{ arithmetic as irarith }
import inca.util.FileUtil

object GenerateIR:
  def inputName(name: ir.Name): ir.Name = ir.Name(s"input_$name")

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
    // Load all mappings from external files
    val domainDecls = prog.content.collect {
      case decl: ProgramContent.DomainDecl => decl
    } ++ extDomains

    // create a new context object
    val ctx = new Context {
      override val rules = prog.content.flatMap {
        case decl@ProgramContent.RelationDecl(name, _, _) => Some(decl -> collectRules(name, prog.content))
        case _ => None
      }.toMap
      override val domains = domainDecls.map {
        case DomainDecl(name, size, Some(qn)) => name -> (size, loadDeclMapping(baseDir, qn.toString))
        case DomainDecl(name, size, None) => name -> (size, Map())
      }.toMap
    }
    given Context = ctx

    ir.Module(ir.Name(name), irLang, compileProgramContents(prog.content))

  def compileProgramContents(content: Seq[ProgramContent])(implicit ctx: Context): Seq[ir.ModuleEntry] =
    content.flatMap(c => compileProgramContent(c)(ctx))

  // If we have a file name for the domain, then we use TString, other TInt
  private def compileDomainType(dom: Domain)(implicit ctx: Context): ir.Type = irarith.TInt

  private def compileAttribute(attr: Attribute)(implicit ctx: Context): ir.Param =
    ir.Param(attr.name, compileDomainType(attr.domain))

  def compileProgramContent(content: ProgramContent)(implicit ctx: Context): Seq[ir.ModuleEntry] = content match
    case decl@ProgramContent.RelationDecl(name, params, options) => compileRelationDecl(decl)
    case ProgramContent.Rule(name, param, body, options) => Seq() // nothing
    case ProgramContent.DomainDecl(name, size, qualifiedFileName) => Seq() // nothing
    case ProgramContent.Directive(dirQualifier) => Seq() // nothing

  def compileRelationDecl(decl: ProgramContent.RelationDecl)(implicit ctx: Context): Seq[ir.ModuleEntry] =
    val edbBodies = if (decl.options.contains(RelationOption.Input)) {
      Seq(ir.Body(Seq(
        ir.ExtensionalCall(GenerateIR.inputName(decl.name), decl.attrs.map(attr => ir.Var(attr.name).arg))
      )))
    } else {
      Seq()
    }
    val params = decl.attrs.map(compileAttribute)
    val rules = ctx.rules(decl)
    val rel = ir.Relation(
      decl.name,
      params,
      edbBodies ++ rules.map(r => compileRule(decl, r))
    )
    Seq(rel)

  private def compileAtom(atom: Atom)(implicit ctx: Context): ir.Atom = atom match
    case Atom.Call(rel, args, neg) => ir.Call(rel, args.map(compileArg), neg)
    case Atom.Compare(lhs, "=", rhs) => ir.Eq(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, "!=", rhs) => ir.Eq(compileTerm(lhs), compileTerm(rhs), true)
    case Atom.Compare(lhs, "<", rhs) => irarith.LT(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, ">", rhs) => irarith.GT(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, "<=", rhs) => irarith.LE(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, ">=", rhs) => irarith.GE(compileTerm(lhs), compileTerm(rhs))
    case Atom.Compare(lhs, "=>", rhs) => ??? // Something with domains

  private def compileArg(term: Term)(implicit ctx: Context): ir.Arg = term match
    case Term.Var(ir.Name("_")) => ir.WildcardArg()
    case _ => compileTerm(term).arg

  private def compileTerm(term: Term)(implicit ctx: Context): ir.Term = term match
    case Term.Var(name) => ir.Var(name)
    case Term.NumberLit(value) => irarith.IntNum(value)
    case Term.StringLit(value) => ???
      // TODO: Get d via "type information" aka domain information
      // val (size, mapping) = domain(d)
      // val idx = mapping(value)
      // irarith.IntNum(idx)

  private def compileRule(decl: ProgramContent.RelationDecl, rule: ProgramContent.Rule)(implicit ctx: Context): ir.Body =
    val relName = decl.name
    val domainInput: Seq[ir.Atom] = Seq() // TODO: We need to add the domain input
    val renameConstraints = decl.attrs.zip(rule.params).map {
      case (attr, term) => ir.Eq(ir.Var(attr.name), compileTerm(term))
    }
    ir.Body(
      domainInput ++ rule.body.map(compileAtom) ++ renameConstraints,
    )