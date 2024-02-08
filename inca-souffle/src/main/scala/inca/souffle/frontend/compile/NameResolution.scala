package inca.souffle.frontend.compile

import inca.ir.typing.Resolvable
import inca.souffle.syntax.ProgramContent.RelationDecl
import inca.souffle.syntax.{Atom, ComponentType, Conjunction, Program, ProgramContent, QualifiedName, Term}


trait NameResolution:
  val ctx = new SouffleContext {}

  def resolveProgram(prog: Program): Unit =
    // register decls, componentdecls and typedecls
    prog.content.foreach(register)
    // resolve contents
    prog.content.foreach(resolveProgramContent)

  def register(content: ProgramContent): Unit =
    content match
      case decl: ProgramContent.TypeDecl => ctx.bindTypeDecl(decl)
      case decl: ProgramContent.RelationDecl => ctx.bindRelationDecl(decl)
      case decl: ProgramContent.ComponentDecl => ctx.bindComponentDecl(decl)
      case init: ProgramContent.ComponentInit => ctx.bindComponentInit(init)
      case _ => ()

  def resolveProgramContent(content: ProgramContent): Unit = content match
    case ProgramContent.Rule(heads, body, queryPlan) =>
      heads.foreach(resolveAtom)
      body.foreach(resolveAtom)
    case fact@ProgramContent.Fact(qualifiedName, args) =>
      ctx.lookupRelationDecl(qualifiedName) match
        case Some(relDecl) => fact.resolved(relDecl)
        case None => throw IllegalArgumentException(s"Could not resolve $qualifiedName for $content")
    case ProgramContent.ComponentDecl(ty, superTys, content) =>
      ctx.scopedTypeContext {
        // what to do with super types?
        ctx.newComponentLevel(ty)
        content.foreach(register)
        content.foreach(resolveProgramContent)
      }
    case compInit@ProgramContent.ComponentInit(n, compType) =>
      // TODO is the input the correct one?
      ctx.lookupComponentDecl(compType) match
        case Some(compDecl) => compInit.resolved(compDecl)
        case None => throw IllegalArgumentException(s"Could not resolve ${compType.n} for $content")
    case dir@ProgramContent.Directive(dirQualifier, name, attrs) =>
      ctx.lookupRelationDecl(name) match
        case Some(relDecl) => dir.resolved(relDecl)
        case None => throw IllegalArgumentException(s"Could not resolve $name for $content")
    case ProgramContent.TypeDecl(name, rhs) => // do nothing
    case ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) => // do nothing

    case ProgramContent.Override(n) => // do nothing?
    case ProgramContent.FunctorDecl(name, params, retType, stateful) => // do nothing
    case ProgramContent.Pragma(option, arg) => // do nothing

  def resolveAtom(atom: Atom): Unit = atom match
    case call@Atom.Call(qualifiedName, args) =>
      ctx.lookupRelationDecl(qualifiedName) match
        case Some(relDecl) => call.resolved(relDecl)
        case None => throw IllegalArgumentException(s"Could not resolve $qualifiedName for $atom")
    case Atom.Not(atom) => resolveAtom(atom)
    case Atom.Disjunction(bodys) => bodys.foreach(_.atoms.foreach(resolveAtom))
    case Atom.LessThan(t1, t2) => // do nothing for the rest
    case Atom.LessThanEqual(t1, t2) =>
    case Atom.GreaterThan(t1, t2) =>
    case Atom.GreaterThanEqual(t1, t2) =>
    case Atom.Equal(t1, t2) =>
    case Atom.Unequal(t1, t2) =>
    case Atom.Match(t1, t2) =>
    case Atom.Contains(t1, t2) =>
    case Atom.True =>
    case Atom.False =>
