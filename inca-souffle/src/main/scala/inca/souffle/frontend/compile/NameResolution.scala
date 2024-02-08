package inca.souffle.frontend.compile

import inca.souffle.syntax.ProgramContent.RelationDecl
import inca.souffle.syntax.{Aggregator, Atom, ComponentType, Program, ProgramContent, Term, Type, TypeDeclConstraint}

import java.awt.Component


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
    case ProgramContent.ComponentDecl(ty, superTys, innerContent) =>
      ctx.scopedTypeContext {
        ctx.newComponentLevel(ty)
        superTys.foreach { superCompType =>
          ctx.lookupComponentDecl(superCompType) match
            case Some(compDecl) => superCompType.resolved(compDecl)
            case None => throw IllegalArgumentException(s"Could not resolve ${superCompType.n} for $content")
        }
        innerContent.foreach(register)
        innerContent.foreach(resolveProgramContent)
      }
    case compInit@ProgramContent.ComponentInit(n, compType) =>
      ctx.lookupComponentDecl(compType) match
        case Some(compDecl) => compInit.resolved(compDecl)
        case None => throw IllegalArgumentException(s"Could not resolve ${compType.n} for $content")
    case dir@ProgramContent.Directive(dirQualifier, name, attrs) =>
      ctx.lookupRelationDecl(name) match
        case Some(relDecl) => dir.resolved(relDecl)
        case None => throw IllegalArgumentException(s"Could not resolve $name for $content")
    case ProgramContent.TypeDecl(name, rhs) => resolveTypeDeclConstraint(rhs)
    case ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) =>
      attrs.foreach { attr =>
        resolveType(attr.ty)
      }
    case ProgramContent.Override(n) => // TODO do nothing?
    case ProgramContent.FunctorDecl(name, params, retType, stateful) => // do nothing
    case ProgramContent.Pragma(option, arg) => // do nothing

  def resolveTypeDeclConstraint(tyDeclConstraint: TypeDeclConstraint): Unit = tyDeclConstraint match
    case TypeDeclConstraint.DefType() => // do nothing
    case TypeDeclConstraint.EqType(ty) => resolveType(ty)
    case TypeDeclConstraint.SubType(ty) => resolveType(ty)
    case TypeDeclConstraint.UnionType(alts) =>
      alts.foreach(resolveType)
    case TypeDeclConstraint.RecordType(rec) =>
      rec.attrs.foreach { attr =>
        resolveType(attr.ty)
      }
    case TypeDeclConstraint.ADTType(alts) =>
      alts.foreach { adtConstr =>
        adtConstr.attrs.foreach { attr =>
          resolveType(attr.ty)
        }
      }

  def resolveType(ty: Type): Unit = ty match
    case tyName@Type.Name(qualName) =>
      ctx.lookupTypeDeclDecl(qualName) match
        case Some(typeDecl) =>  tyName.resolved(typeDecl)
        case None => throw IllegalArgumentException(s"Could not resolve type $qualName")
    case Type.Number => // do nothing
    case Type.Symbol => // do nothing
    case Type.Unsigned => // do nothing
    case Type.Float => // do nothin

  def resolveAtom(atom: Atom): Unit = atom match
    case call@Atom.Call(qualifiedName, args) =>
      ctx.lookupRelationDecl(qualifiedName) match
        case Some(relDecl) => call.resolved(relDecl)
        case None => throw IllegalArgumentException(s"Could not resolve $qualifiedName for $atom")
    case Atom.Not(atom) => resolveAtom(atom)
    case Atom.Disjunction(bodys) => bodys.foreach(_.atoms.foreach(resolveAtom))
    case Atom.LessThan(t1, t2) =>
      resolveTerm(t1)
      resolveTerm(t2)
    case Atom.LessThanEqual(t1, t2) =>
      resolveTerm(t1)
      resolveTerm(t2)
    case Atom.GreaterThan(t1, t2) =>
      resolveTerm(t1)
      resolveTerm(t2)
    case Atom.GreaterThanEqual(t1, t2) =>
      resolveTerm(t1)
      resolveTerm(t2)
    case Atom.Equal(t1, t2) =>
      resolveTerm(t1)
      resolveTerm(t2)
    case Atom.Unequal(t1, t2) =>
      resolveTerm(t1)
      resolveTerm(t2)
    case Atom.Match(t1, t2) =>
      resolveTerm(t1)
      resolveTerm(t2)
    case Atom.Contains(t1, t2) =>
      resolveTerm(t1)
      resolveTerm(t2)
    case Atom.True => // do nothing
    case Atom.False => // do nothing

  def resolveTerm(t: Term): Unit = t match
    case constr@Term.Constr(qualifiedName, args) =>
      ctx.lookupADTConstructor(qualifiedName) match {
        case Some(typeDecl) => constr.resolved(typeDecl)
        case None => throw IllegalArgumentException(s"Could not resolve ADT constructor $qualifiedName for $t")
      }
      args.foreach(resolveTerm)
    case Term.Parens(t) =>
      resolveTerm(t)
    case Term.TypeCast(t, ty) =>
      resolveTerm(t)
      resolveType(ty)
    case Term.AggregatorTerm(agg) => resolveAggregator(agg)
    case Term.IntrinsicFunctorApp(f, args) => args.foreach(resolveTerm)
    case Term.UserDefFunctorApp(f, args) => args.foreach(resolveTerm)
    case Term.Unary(op, t) => resolveTerm(t)
    case Term.Binary(t1, op, t2) =>
      resolveTerm(t1)
      resolveTerm(t2)
    case Term.Var(name) => // do nothing
    case Term.StringLit(s) =>
    case Term.NumberLit(n) =>
    case Term.UnsignedLit(n) =>
    case Term.FloatLit(f) =>
    case Term.Nil =>
    case Term.List(s) =>

  def resolveAggregator(agg: Aggregator): Unit = agg match
    case Aggregator.Max(t, atoms) =>
      resolveTerm(t)
      atoms.foreach(resolveAtom)
    case Aggregator.Mean(t, atoms) =>
      resolveTerm(t)
      atoms.foreach(resolveAtom)
    case Aggregator.Min(t, atoms) =>
      resolveTerm(t)
      atoms.foreach(resolveAtom)
    case Aggregator.Sum(t, atoms) =>
      resolveTerm(t)
      atoms.foreach(resolveAtom)
    case Aggregator.Count(atoms) =>
      atoms.foreach(resolveAtom)
    case Aggregator.Range(begin, end, step) =>
      resolveTerm(begin)
      resolveTerm(end)
      step.foreach(resolveTerm)
