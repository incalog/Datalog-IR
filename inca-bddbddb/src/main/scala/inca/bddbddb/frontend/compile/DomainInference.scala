package inca.bddbddb.frontend.compile

import inca.bddbddb.syntax.ProgramContent.DomainDecl
import inca.bddbddb.syntax.{Atom, Domain, Program, ProgramContent, Term}
import inca.ir.Name

// Infer the domain for each term in a program.
// We need this information to resolve strings to their integer representation
class DomainInference:
  var domains: Map[Name, DomainDecl] = Map()
  var rels: Map[Name, ProgramContent.RelationDecl] = Map()

  // Current variables in scope
  var vars: Map[Name, Domain] = Map()

  def scoped[A](f: => A): A = {
    val oldvars = this.vars
    try {
      val a = f
      a
    } finally {
      this.vars = oldvars
    }
  }

  def inferProgram(prog: Program): Unit =
    // Collect all domains and relations
    prog.content.foreach {
      case ProgramContent.DomainDecl(name, attrs, options) =>
        if domains.contains(name) then
          throw new Exception(s"Domain $name already declared")
        domains += (name -> ProgramContent.DomainDecl(name, attrs, options))
      case ProgramContent.RelationDecl(name, attrs, options) =>
        if rels.contains(name) then
          throw new Exception(s"Relation $name already declared")
        rels += (name -> ProgramContent.RelationDecl(name, attrs, options))
      case _ => // nothing
    }

    prog.content.foreach(inferProgramContent)

  def inferProgramContent(content: ProgramContent): Unit = content match
    case ProgramContent.Rule(name, params, body, options) =>
      scoped {
        val relDecl = rels(name)
        inferCall(name, params)
        body.map(inferAtom)
      }
    case _ => // nothing

  private def inferCall(rel: Name, args: Seq[Term]): Unit =
    val relDecl = rels(rel)
    if args.length != relDecl.attrs.length then
      throw new Exception(s"Relation $rel expects ${relDecl.attrs.length} arguments, got ${args.length}")
    args.zip(relDecl.attrs).foreach {
      case (arg, attr) => inferTerm(arg, attr.domain)
    }

  def inferAtom(atom: Atom): Unit = atom match
    case Atom.Call(rel, args, neg) => inferCall(rel, args)
    case Atom.Compare(lhs, op, rhs) =>
      (lhs, rhs) match
        case (lhs@Term.Var(name), rhs) =>
          val domain = vars(name)
          inferTerm(rhs, domain)
        case (lhs, rhs@Term.Var(name)) =>
          val domain = vars(name)
          inferTerm(lhs, domain)
        case _ => // ignore all other cases nothing

  def inferTerm(term: Term, domain: Domain): Unit = term match
    case Term.Var(name) =>
      vars += (name -> domain)
      term.target = Some(domain)
    case _ =>
      term.target = Some(domain)