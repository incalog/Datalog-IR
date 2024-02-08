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
import inca.souffle.frontend.compile.NameResolution

import scala.annotation.tailrec

/**
 * Things to consider in general:
 * - We need a way to distinguish edb from idb calls (decl without rules are edb ?)
 * - Answer: Rules where an input directive is given is edb
 *
 * Things to consider regarding components:
 * 1. Components can call relations defined outside of their scope
 * 2. We need to prefix calls to relations inside the component, but only, if they are defined inside the component
 * 3. The same as above also holds for types
 *
 */

class GenerateIR {
  val irLang: Language = new Language(Set(ir.BaseIR)
    + irarith.IR + block.IR + bool.IR + irdata.IR + irmatch.IR
    + demand.IR + disjunction.IR + irnot.IR + irset.IR + irmap.IR + irstring.IR + irtuple.IR
    + iragg.IR + iraggset.IR + irtype.IR
  )
  val gensym: Gensym = new Gensym()

  var types: Map[String, ir.Type] = Map()
  var rules: Map[ProgramContent.RelationDecl, Seq[ProgramContent]] = Map()

  var rulePrefixes: Map[ProgramContent.RelationDecl, Seq[String]] = Map()

  def compileProgram(prog: Program, name: String): ir.Module =
    val nameResolution = new NameResolution {}
    nameResolution.resolveProgram(prog)

    rulePrefixes = collectPrefixes(prog.content)
    rules = collectRules(prog.content)

    ir.Module(ir.Name(name), irLang, compileProgramContents(prog.content))

  private def collectTypeAliases(content: Seq[ProgramContent]): Map[String, ir.Type] =
    var types: Map[String, ir.Type] = Map()
    content.foreach {
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.DefType()) =>
        types += name -> irstring.TString
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.EqType(ty)) =>
        types += name -> (ty match
          case Type.Number => irarith.TInt
          case Type.Symbol => irstring.TString
          case Type.Unsigned => irarith.TInt
          case Type.Float => irarith.TDouble
          case Type.Name(qn) => types(qualifiedNameToIrName(qn).name))
      case ProgramContent.TypeDecl(name, TypeDeclConstraint.ADTType(alts)) =>
        types += name -> irdata.TData(ir.Name(name))
      case _ => //
    }
    types

  private def collectPrefixes(content: Seq[ProgramContent], prefix: Seq[String] = Seq()): Map[ProgramContent.RelationDecl, Seq[String]] =
    var declToPrefix: Map[ProgramContent.RelationDecl, Seq[String]] = Map()
    content.foreach {
      case decl: ProgramContent.RelationDecl =>
        declToPrefix += (decl -> prefix)
      case compInit@ProgramContent.ComponentInit(n, compType) =>
        val compDecl@ProgramContent.ComponentDecl(_, _, compContent) = compInit.target.get
        declToPrefix ++= collectPrefixes(compContent, prefix :+ n)
      case _ => // nothing
    }
    declToPrefix

  private def collectRules(content: Seq[ProgramContent]): Map[ProgramContent.RelationDecl, Seq[ProgramContent]] =
    var rules: Map[ProgramContent.RelationDecl, Seq[ProgramContent]] = Map()
    content.foreach {
      case rule@ProgramContent.Rule(heads, _, _) =>
        rules ++= heads.map {
          case call: Atom.Call =>
            val relDecl = call.target.get
            val existingRules = rules.getOrElse(relDecl, Seq())
            relDecl -> (existingRules :+ rule)
        }
      case fact@ProgramContent.Fact(_, _) =>
        val relDecl = fact.target.get
        val existingRules = rules.getOrElse(relDecl, Seq())
        rules += relDecl -> (existingRules :+ fact)
      case compDecl@ProgramContent.ComponentDecl(_, _, compContent) =>
        rules ++= collectRules(compContent)
      case _ => // nothing
    }
    rules

  private def compileProgramContents(contents: Seq[ProgramContent]): Seq[ir.ModuleEntry] =
    contents.flatMap(compileProgramContent)

  private def compileProgramContent(content: ProgramContent): Seq[ir.ModuleEntry] =
    content match
      case relDecl@ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) =>
        compileRelationDecl(relDecl)
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
        throw IllegalStateException(s"Record types are not supported: $content")
      case ProgramContent.TypeDecl(name, _) =>
        Seq() // nothing
      case ProgramContent.ComponentDecl(_, _, compContent) =>
        compileProgramContents(compContent)
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

  private def namesToIrName(ns: Seq[String]): ir.Name =
    ir.Name(ns.mkString("$"))

  private def qualifiedNameToIrName(qn: QualifiedName): ir.Name =
    namesToIrName(qn.ns)

  private def rName(relationName: String, prefix: Seq[String]): ir.Name =
    ir.Name((prefix :+ relationName).mkString("$"))

  private def ruleHasName(rule: ProgramContent, relName: String): Boolean = rule match
    case ProgramContent.Rule(heads, _, _) =>
      heads.exists {
        case Atom.Call(QualifiedName(ns), _) if ns.last == relName => true
        case _ => false
      }
    case ProgramContent.Fact(QualifiedName(ns), args) if ns.last == relName => true
    case _ => false


  private def compileRelationDecl(decl: ProgramContent.RelationDecl): Seq[ir.ModuleEntry] =
    // TODO: Do something with qualifiers and choiceDomain
    val ProgramContent.RelationDecl(names, attrs, qualifiers, choiceDomain) = decl

    names.map { relName =>
      val params = attrs.map(compileAttribute)

      // Find all rules relevant for this relation
      val rulesForRelation = rules(decl).filter(r => ruleHasName(r, relName))
      val prefix = rulePrefixes(decl)

      // TODO: Introduce prefix
      val isExtensionalRelation = rulesForRelation.isEmpty // TODO: Fix me based on input directive
      if (isExtensionalRelation) {
        ir.ExtensionalRelation(rName(relName, prefix), params)
      } else {
        ir.Relation(rName(relName, prefix), params, rulesForRelation.map {
          case r: ProgramContent.Rule => compileRule(decl, r, relName)
          case f: ProgramContent.Fact => compileFact(decl, f)
          case c => throw IllegalStateException(s"Found unexpected content $c for relation $relName")
        })
      }
    }

  private def compileAttribute(attr: Attribute): ir.Param =
    ir.Param(cleanParamName(attr.name), compileType(attr.ty))

  private def compileRule(decl: ProgramContent.RelationDecl, rule: ProgramContent.Rule, relName: String): ir.Body =
    val ProgramContent.Rule(heads, atoms, queryPlanOption) = rule
    val headTerms = heads.map {
      case Atom.Call(QualifiedName(ns), terms) if ns.last == relName => terms
      case _ => Seq()
    }.headOption.getOrElse(Seq())

    // rules might use other names than relations
    val renameAtoms = headTerms.zip(decl.attrs).map { (headTerm, attr) =>
      ir.Eq(compileTerm(headTerm), ir.Var(cleanParamName(attr.name)))
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
    case call@Atom.Call(QualifiedName(ns), args) =>
      val compileArgs = args.map(compileTerm).map(_.arg)
      val decl = call.target.get
      val prefix = rulePrefixes(decl)
      // TODO: Fix me
      // val isExtensional = rules(componentPrefix :+ irName.name).isEmpty
      //if (isExtensional)
      //  ir.ExtensionalCall(irName, compileArgs)
      //else
      ir.Call(namesToIrName(prefix :+ ns.last), compileArgs)
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
      irdata.Construct(qualifiedNameToIrName(name), args.map(compileTerm))
    case Term.Parens(t) =>
      // TODO: Is it fine to just ignore these ?
      compileTerm(t)
    case Term.TypeCast(t, ty) =>
      ir.Cast(compileTerm(t), compileType(ty))
    case Term.AggregatorTerm(agg) => ???
    case Term.IntrinsicFunctorApp(f, args) => ???
    case Term.UserDefFunctorApp(f, args) => ???

    case Term.Unary(UnOp.Neg, t) => ???
    case Term.Unary(UnOp.Bnot, t) => ???
    case Term.Unary(UnOp.Lnot, t) => irbool.BoolNot(compileTerm(t))

    case Term.Binary(t1, BinOp.Add, t2) => irarith.Add(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Sub, t2) => irarith.Sub(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Mul, t2) => irarith.Mul(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Div, t2) => irarith.Div(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Rem, t2) => ???
    case Term.Binary(t1, BinOp.Pow, t2) => ???
    case Term.Binary(t1, BinOp.Land, t2) => irbool.BoolAnd(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Lor, t2) => irbool.BoolOr(compileTerm(t1), compileTerm(t2))
    case Term.Binary(t1, BinOp.Lxor, t2) =>
      val a = compileTerm(t1)
      val b = compileTerm(t2)
      irbool.BoolAnd(irbool.BoolOr(a, b), irbool.BoolNot(irbool.BoolAnd(a, b)))
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
