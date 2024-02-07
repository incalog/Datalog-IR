package inca.souffle.frontend.compile

import inca.ir
import inca.ir.Language
import inca.ir.extension.{block, bool, demand, disjunction, typeparam}
import inca.souffle.syntax.{ADTConstructor, Atom, Attribute, BinOp, ComponentType, Program, ProgramContent, QualifiedName, Qualifier, Term, Type, TypeDeclConstraint, UnOp}
import inca.util.Gensym
import inca.ir.extension.map as irmap
import inca.ir.extension.not as irnot
import inca.ir.extension.set as irset
import inca.ir.extension.string as irstring
import inca.ir.extension.tuple as irtuple
import inca.ir.extension.aggregate as iragg
import inca.ir.extension.aggregateset as iraggset
import inca.ir.extension.arithmetic as irarith
import inca.ir.extension.arithmetic.IntNum
import inca.ir.extension.block as irblock
import inca.ir.extension.bool as irbool
import inca.ir.extension.bool.{BoolFalse, BoolTrue}
import inca.ir.extension.data as irdata
import inca.ir.extension.disjunction as irdis
import inca.ir.extension.datamatch as irmatch
import inca.ir.extension.typeparam as irtype

class GenerateIR {
  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + bool.IR + irdata.IR + irmatch.IR
    + demand.IR + disjunction.IR + irnot.IR + irset.IR + irmap.IR + irstring.IR + irtuple.IR
    + iragg.IR + iraggset.IR + irtype.IR
  )

  val gensym: Gensym = new Gensym()

  // For each component collect all inits
  var componentDecl: Map[Seq[String], ProgramContent.ComponentDecl] = Map()
  var componentInit: Map[ProgramContent.ComponentInit, Seq[String]] = Map()

  var types: Map[String, ir.Type] = Map()
  var ruleMapping: Map[String, Seq[ProgramContent]] = Map()
  
  def scoped[A](f: => A): A = {
    val oldTypes = types
    try {
      val a = f
      a
    } finally {
      types = oldTypes
    }
  }
  
  def compileProgram(prog: Program, name: String): ir.Module =
    componentDecl = collectComponentDecl(prog.content)
    componentInit = collectComponentInit(prog.content)
    types = collectTypeAliases(prog.content)
    ruleMapping = collectRules(prog.content)
    ir.Module(ir.Name(name), irLang, prog.content.flatMap(c => compileProgramContent(c, ruleMapping, Seq())))

  private def collectTypeAliases(content: Seq[ProgramContent]): Map[String, ir.Type] =
    // TODO: Make sure that we first define all types before we resolve aliases
    var types: Map[String, ir.Type] = Map()
    content.foreach {
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.DefType()) =>
        types += name -> irstring.TString
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.EqType(ty)) =>
        types += name -> compileType(ty)
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(alts)) =>
        types += name -> irdata.TData(ir.Name(name))
      case _ => // 
    }
    types

  private def collectComponentDecl(content: Seq[ProgramContent], prefix: Seq[String] = Seq()): Map[Seq[String], ProgramContent.ComponentDecl] =
    var compDecl: Map[Seq[String], ProgramContent.ComponentDecl] = Map()
    content.foreach {
      case comp@ProgramContent.ComponentDecl(ty, superTys, compContent) =>
        val ComponentType(compName, argTypes) = ty
        val newPrefix = prefix :+ compName
        compDecl += (newPrefix -> comp)
        compDecl ++ collectComponentDecl(compContent, newPrefix)
      case _ => // nothing
    }
    compDecl

  private def collectComponentInit(content: Seq[ProgramContent], prefix: Seq[String] = Seq()): Map[ProgramContent.ComponentInit, Seq[String]] =
    var compInit: Map[ProgramContent.ComponentInit, Seq[String]] = Map()
    content.foreach {
      case comp@ProgramContent.ComponentDecl(ty, superTys, compContent) =>
        val ComponentType(compName, argTypes) = ty
        val newPrefix = prefix :+ compName
        compInit ++ collectComponentInit(compContent, newPrefix)
      case init@ProgramContent.ComponentInit(_, ty) =>
        val ComponentType(compName, argTypes) = ty
        compInit += (init -> (prefix :+ compName))
      case _ => // nothing
    }
    compInit

  private def collectRules(content: Seq[ProgramContent]): Map[String, Seq[ProgramContent]] =
    var rules: Map[String, Seq[ProgramContent]] = Map()
    content.foreach {
      case decl: ProgramContent.RelationDecl =>
        decl.names.foreach { name =>
          val newRules = rules.getOrElse(name, Seq())
          rules += (name -> newRules)
        }
      case rule: ProgramContent.Rule =>
        rule.heads.foreach {
          case Atom.Call(qn, vars: Seq[Term.Var]) =>
            val qName = qualifiedNameToIrName(qn)
            val newRules = rules.getOrElse(qName.name, Seq()) :+ rule
            rules += (qName.name -> newRules)
          case head =>
            throw IllegalStateException(s"Can not handle head atom: $head")
        }
      case fact@ProgramContent.Fact(qn, _) =>
        val qname = qualifiedNameToIrName(qn)
        val newRules = rules.getOrElse(qname.name, Seq()) :+ fact
        rules += (qname.name -> newRules)
      case _ => // nothing
    }
    rules

  private def compileProgramContent(content: ProgramContent, rules: Map[String, Seq[ProgramContent]], prefix: Seq[String]): Seq[ir.ModuleEntry] =
    content match
      case relDecl@ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) =>
        compileRelationDecl(relDecl, prefix, rules)
      case ProgramContent.Rule(heads, body, queryPlan) =>
        Seq() // nothing, handled by ProgramContent.RelationDecl
      case ProgramContent.Fact(name, args) =>
        Seq() // nothing, handled by ProgramContent.RelationDecl
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(alts)) =>
        val dataDef = irdata.DataDefinition(ir.Name(name))
        val caseDefs = alts.map { case ADTConstructor(name, attrs) => 
            irdata.CaseDefinition(ir.Name(name), attrs.map(a => compileType(a.ty)), irdata.TData(dataDef.name))
        }
        dataDef +: caseDefs
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.SubType(ty)) =>
        throw IllegalStateException(s"Subtypes are not supported: $content")
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.RecordType(alts)) =>
        throw IllegalStateException(s"Union types are not supported: $content")
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.UnionType(alts)) =>
        throw IllegalStateException(s"RecordType types are not supported: $content")
      case ProgramContent.TypeDecl(name, _) =>
        Seq() // nothing
      case init@ProgramContent.ComponentInit(instanceName, compTy) =>
        val ComponentType(compName, argTypes) = compTy
        val compPrefix = componentInit(init)
        val compDecl = componentDecl(compPrefix)
        val newRules = collectRules(compDecl.content)
        scoped {
          types ++= collectTypeAliases(compDecl.content)
          compDecl.content.flatMap { c =>
            compileProgramContent(c, newRules, prefix ++ Seq(instanceName))
          }
        }
      case ProgramContent.ComponentDecl(ty, superTys, content) =>
        Seq() // nothing, handled by ProgramContent.ComponentInit
      case _ =>
        Seq()
      /*
      case ProgramContent.Directive(dirQualifier, name, attrs) => ???
      case ProgramContent.Override(n) => ???
      case ProgramContent.FunctorDecl(name, params, retType, stateful) => ???
      case ProgramContent.Pragma(option, arg) => ???*/

  private def cleanParamName(name: String): ir.Name =
    // We know that $ is disallowed as souffle variable name
    ir.Name(s"$name$$param")

  private def qualifiedNamesToIrName(ns: Seq[String]): ir.Name =
    ir.Name(ns.mkString("$"))

  private def qualifiedNameToIrName(qn: QualifiedName): ir.Name =
    qualifiedNamesToIrName(qn.ns)

  private def rName(relationName: String, prefix: Seq[String]): ir.Name =
    ir.Name((prefix :+ relationName).mkString("$"))

  private def compileRelationDecl(decl: ProgramContent.RelationDecl, prefix: Seq[String], rules: Map[String, Seq[ProgramContent]]): Seq[ir.ModuleEntry] =
    // TODO: Do something with qualifiers and choiceDomain
    val ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) = decl
    names.map { relName =>
      val params = attrs.map(compileAttribute)
      // Fixme: decls without any body are extensional. Correct ?
      val rulesForRel = rules(relName)
      
      // Extend our global map of rules to differentiate edb and idb calls
      ruleMapping += qualifiedNamesToIrName(prefix :+ relName).name -> rulesForRel

      val isExtensionalRelation = rulesForRel.isEmpty
      if (isExtensionalRelation) {
        ir.ExtensionalRelation(rName(relName, prefix), params)
      } else {
        ir.Relation(rName(relName, prefix), params, rulesForRel.map {
          case r: ProgramContent.Rule => compileRule(decl, r)
          case f: ProgramContent.Fact => compileFact(decl, f)
          case c => throw IllegalStateException(s"Found unexpected content $c for relation $relName")
        })
      }
    }

  private def compileAttribute(attr: Attribute): ir.Param =
    ir.Param(cleanParamName(attr.name), compileType(attr.ty))

  private def compileRule(decl: ProgramContent.RelationDecl, rule: ProgramContent.Rule): ir.Body =
    val ProgramContent.Rule(head, atoms, queryPlanOption) = rule
    val Seq(Atom.Call(_, vars: Seq[Term.Var])) = head
    // rules might use other names than relations
    val renameAtoms = vars.zip(decl.attrs).map { (headVar, attr) =>
      ir.Eq(ir.Var(ir.Name(headVar.name)), ir.Var(cleanParamName(attr.name)))
    }
    ir.Body(atoms.map(compileAtom) ++ renameAtoms)

  private def compileFact(decl: ProgramContent.RelationDecl, fact: ProgramContent.Fact): ir.Body =
    val ProgramContent.Fact(name, args) = fact
    ir.Body(
      args.zip(decl.attrs).map { (arg, attr) =>
        ir.Eq(ir.Var(cleanParamName(attr.name)), compileTerm(arg))
      }
    )


  private def compileAtom(atom: Atom): ir.Atom = atom match
    case Atom.Not(atom) =>
      irnot.Not(compileAtom(atom))
    case Atom.Call(qn, args) =>
      val irName = qualifiedNameToIrName(qn)
      val compileArgs = args.map(compileTerm).map(_.arg)
      val isExtensional = ruleMapping(irName.name).isEmpty
      if (isExtensional)
        ir.ExtensionalCall(irName, compileArgs)
      else
        ir.Call(irName, compileArgs)  
    case Atom.Disjunction(bodys) =>
      irdis.Disjunction(bodys.map(b => irdis.DisjunctionAlternative(b.atoms.map(compileAtom))))
    case Atom.LessThan(t1, t2) =>
      irarith.LT(compileTerm(t1), compileTerm(t2))
    case Atom.LessThanEqual(t1, t2) =>
      irarith.LE(compileTerm(t1), compileTerm(t2))
    case Atom.GreaterThan(t1, t2) =>
      irarith.GT(compileTerm(t1), compileTerm(t2))
    case Atom.GreaterThanEqual(t1, t2) =>
      irarith.GE(compileTerm(t1), compileTerm(t2))
    case Atom.Equal(t1, t2) =>
      ir.Eq(compileTerm(t1), compileTerm(t2))
    case Atom.Unequal(t1, t2) =>
      ir.Eq(compileTerm(t1), compileTerm(t2), true)
    case Atom.Match(t1, t2) => ???
    case Atom.Contains(t1, t2) => ???
    case Atom.True => ir.Eq(BoolTrue, BoolTrue)
    case Atom.False => ir.Eq(BoolTrue, BoolFalse)

  private def compileTerm(term: Term): ir.Term = term match
    case Term.Var(name) => ir.Var(ir.Name(name))
    case Term.StringLit(s) => irstring.StringLit(s)
    case Term.NumberLit(n) => irarith.IntNum(n)
    case Term.UnsignedLit(n) => irarith.IntNum(n.toInt)
    case Term.FloatLit(f) => irarith.DoubleNum(f)
    case Term.Nil => ???
    case Term.List(s) => ???
    case Term.Constr(name, args) => 
      irdata.Construct(ir.Name(name), args.map(compileTerm))
    case Term.Parens(t) => ???
    case Term.TypeCast(t, ty) => ???
    case Term.AggregatorTerm(agg) => ???
    case Term.IntrinsicFunctorApp(f, args) => ???
    case Term.UserDefFunctorApp(f, args) => ???

    case Term.Unary(UnOp.Neg, t) => ???
    case Term.Unary(UnOp.Bnot, t) => ???
    case Term.Unary(UnOp.Lnot, t) => ???

    case Term.Binary(t1, BinOp.Add, t2) => irarith.Add(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Sub, t2) => irarith.Sub(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Mul, t2) => irarith.Mul(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Div, t2) => irarith.Div(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Rem, t2) => ???
    case Term.Binary(t1, BinOp.Pow, t2) => ???
    case Term.Binary(t1, BinOp.Land, t2) => irbool.BoolAnd(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Lor, t2) => irbool.BoolOr(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Lxor, t2) => ???
    case Term.Binary(t1, BinOp.Band, t2) => ???
    case Term.Binary(t1, BinOp.Bor, t2) => ???
    case Term.Binary(t1, BinOp.Bxor, t2) => ???
    case Term.Binary(t1, BinOp.Bshl, t2) => ???
    case Term.Binary(t1, BinOp.Bshr, t2) => ???
    case Term.Binary(t1, BinOp.Bshru, t2) => ???

  private def compileType(ty: Type): ir.Type = ty match
    case Type.Number => irarith.TInt
    case Type.Symbol => irstring.TString
    case Type.Unsigned => irarith.TInt
    case Type.Float => irarith.TDouble
    case Type.Name(qn) => types(qualifiedNameToIrName(qn).name)
}
