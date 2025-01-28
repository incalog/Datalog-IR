package inca.ir.printer
import inca.ir.{Arg, TAny, TNothing, Var, Eq, Call, ExtensionalCall, Cast, Atom, Body, ExtensionalRelation, ExtensionalRelationSubstitution, Import, Module, ModuleEntry, Name, Param, ProvideExtensionalRelation, ProvideRelation, Ref, Relation, RelationSubstitution, RequireExtensionalRelation, RequireRelation, Substitution, Term, TermArg, TermType, Type, WildcardArg}

trait BaseIRPrinter extends GenericPrinter:
  override def prettyPrint(name: Name): String =
    name.name

  override def prettyPrint(ref: Ref[?]): String =
    prettyPrint(ref.name)

  override def prettyPrint(module: Module): String =
    val Module(name, lang, contents) = module
    val features = if lang.features.nonEmpty then s"(with ${lang.features.map(_.name).mkString(",")})" else ""
    val con = contents.map(prettyPrint).mkString("\n")
    s"module $name $features\n$con"

  override def prettyPrint(moduleEntry: ModuleEntry): String = moduleEntry match
    case Import(module, as, subst) =>
      if subst.nonEmpty then
        s"import ${prettyPrint(module.name)} as ${prettyPrint(as)} with { ${subst.map(prettyPrint).mkString(", ")} }"
      else
        s"import ${prettyPrint(module.name)} as ${prettyPrint(as)}"
    case RequireRelation(name, params) =>
      s"require ${prettyPrint(name)}(${params.map(prettyPrint).mkString(", ")})"
    case RequireExtensionalRelation(name, params) =>
      s"require ext ${prettyPrint(name)}(${params.map(prettyPrint).mkString(", ")})"
    case ProvideRelation(exportRef, params) =>
      s"provide ${prettyPrint(exportRef)}(${params.map(prettyPrint).mkString(", ")})"
    case ProvideExtensionalRelation(exportRef, params) =>
      s"provide ext ${prettyPrint(exportRef)}(${params.map(prettyPrint).mkString(", ")})"
    case ExtensionalRelation(name, params) =>
      s"ext ${prettyPrint(name)}${params.map(prettyPrint).mkString("(", ", ", ")")}"
    case Relation(name, params, bodies) =>
      val prefix = s"${prettyPrint(name)}${params.map(prettyPrint).mkString("(", ", ", ")")}"
      if (bodies.isEmpty)
        s"$prefix = nil"
      else
        s"$prefix ${bodies.map(prettyPrint).mkString("{\n", "\n} or {\n", "\n}")}"
    case _ => moduleEntry.toString // fallback

  override def prettyPrint(subst: Substitution[?, ?]): String = subst match
    case ExtensionalRelationSubstitution(to, toParams, from, fromParams) =>
      val lhs = s"${prettyPrint(to)}(${toParams.map(prettyPrint).mkString(", ")})"
      val rhs = s"${prettyPrint(from)}(${fromParams.map(prettyPrint).mkString(", ")})"
      s"$lhs = ext $rhs"
    case RelationSubstitution(to, toParams, from, fromParams) =>
      val lhs = s"${prettyPrint(to)}(${toParams.map(prettyPrint).mkString(", ")})"
      val rhs = s"${prettyPrint(from)}(${fromParams.map(prettyPrint).mkString(", ")})"
      s"$lhs = $rhs"
    case _ => subst.toString // fallback

  override def prettyPrint(body: Body): String =
    s"${body.atoms.map(prettyPrint).mkString("\t", "\n\t", "")}"

  override def prettyPrint(arg: Arg): String = arg match
    case TermArg(t) => prettyPrint(t)
    case w@WildcardArg() if w.typ.isEmpty => "_"
    case w@WildcardArg() => s"_: ${prettyPrint(w.typ.get)}"
    case _ => arg.toString // fallback

  override def prettyPrint(termTy: TermType): String =
    val TermType(ty, mode) = termTy
    if (mode.isBoundCouldBeBinding)
      s"<$ty<"
    else if (mode.isBound)
      s"<${prettyPrint(ty)}>"
    else if (mode.isBinding)
      s">${prettyPrint(ty)}<"
    else if (mode.isCollapse)
      s"<_>"
    else
      throw IllegalStateException(s"Unknown mode $mode")

  override def prettyPrint(ty: Type): String = ty match
    case TAny => "TAny"
    case TNothing => "TNothing"
    case _ => ty.toString // fallback

  override def prettyPrint(param: Param): String =
    val Param(name, ty) = param
    s"${prettyPrint(name)}: ${prettyPrint(ty)}"

  override def prettyPrint(atom: Atom): String = atom match
    case Call(ref, args, neg) =>
      val negPrefix = if (neg) "~" else ""
      s"$negPrefix${prettyPrint(ref)}${args.map(prettyPrint).mkString("(", ", ", ")")}"
    case ExtensionalCall(ref, args, neg) =>
      val negPrefix = if (neg) "~" else ""
      s"ext $negPrefix${prettyPrint(ref)}${args.map(prettyPrint).mkString("(", ", ", ")")}"
    case Eq(lhs, rhs, neg) =>
      val op = if (neg) "!=" else "=="
      s"${prettyPrint(lhs)} $op ${prettyPrint(rhs)}"
    case _ => atom.toString // fallback

  override def prettyPrint(term: Term): String = term match
    case Var(ref) => term.typ match
      case Some(ty) => s"${prettyPrint(ref)}: ${prettyPrint(ty)}"
      case _ => prettyPrint(ref)
    case Cast(t, ty) =>
      if (t.typ.exists(_.ty == ty))
        prettyPrint(t)
      else
        s"Cast(${prettyPrint(t)}, ${prettyPrint(ty)})"
    case _ => term.toString // fallback

