package inca.souffle.backend

import inca.ir
import inca.ir.{RefByName, TermArg}
import inca.ir.extension.aggregate.{AggregateColumnArg, AggregationOperatorBuiltIn, AggregationOperatorUserDefined}
import inca.ir.extension.data.{CaseDefinition, TData}
import inca.ir.extension.{data, string, aggregate as agg, arithmetic as arith}
import inca.souffle.frontend.{SouffleInputHint, SouffleOutputHint, SouffleQueryPlanHint}
import inca.souffle.frontend.compile.GenerateIR
import inca.souffle.syntax.*
import inca.souffle.syntax.Comparator.EQ

// TODO what is output? Need main hint
// Core + Arithmetic + String + Data
object GenerateSouffle:

  def compileModule(module: ir.Module): Program =
    val compilableFeatures = Set(ir.BaseIR, arith.IR, string.IR, data.IR, agg.IR)
    val illegalFeatures = module.lang.features -- compilableFeatures
    if (illegalFeatures.nonEmpty)
      throw IllegalArgumentException(s"Cannot compile module containing the following features: ${illegalFeatures.mkString(", ")}")

    val contents = module.contents.flatMap {
      case rel@ir.Relation(name, params, bodies) =>
        val attrs = params.map { p =>
          Attribute(cleanName(p.name), compileType(p.ty))
        }
        val relDecl = ProgramContent.RelationDecl(Seq(cleanName(name)), attrs, Seq(), None)
        val head = Atom.Call(QualifiedName(Seq(cleanName(name))), params.map(p => Term.Var(cleanName(p.name))))

        val conjunctions = bodies.map(compileBody)
        val queryPlans = bodies.map(_.getHint[SouffleQueryPlanHint](SouffleQueryPlanHint))
        val rules = conjunctions.zip(queryPlans).map { (atoms, queryPlanHintOption) =>
          ProgramContent.Rule(Seq(head), Atom.Disjunction(Seq(atoms)), queryPlanHintOption.map(_.qp))
        }

        // TODO: Although this is correct for souffle programs, we currently expect all outputs for IncA programs
        //if (rel.hasHint(SouffleOutputHint)) {
        val outputDirective = ProgramContent.Directive(DirectiveQualifier.Output, List(qualifyName(name)), Map())
        Seq(relDecl, outputDirective) ++ rules
        //} else {
        //  relDecl +: rules
        //}

      case edb@ir.ExtensionalRelation(name, params) =>
        val attrs = params.map { p =>
          Attribute(cleanName(p.name), compileType(p.ty))
        }
        val relDecl = ProgramContent.RelationDecl(Seq(cleanName(name)), attrs, Seq(), None)
        val directiveAttrs = edb.getHint[SouffleInputHint](SouffleInputHint).map(_.attrs).getOrElse(Map())
        val inputDirective = ProgramContent.Directive(DirectiveQualifier.Input, List(qualifyName(name)), directiveAttrs)
        Seq(relDecl, inputDirective)
      case data.DataDefinition(name) =>
        val cases = module.contents.collect {
          case cd@CaseDefinition(_, _, td) if td.ref.name == name => cd
        }
        val adtBranches = cases.map {
          case data.CaseDefinition(name, args, TData(RefByName(_))) =>
            val cotrArgs = args.zipWithIndex.map { case(ty, idx) =>
              Attribute(s"param_$idx", compileType(ty))
            }
            ADTConstructor(cleanName(name), cotrArgs)
        }
        val adtDef = TypeDeclConstraint.ADTType(adtBranches)
        val typeDecl = ProgramContent.TypeDecl(cleanName(name), adtDef)
        Seq(typeDecl)
      case data.CaseDefinition(name, args, data) => Seq()

    }
    Program(contents)

  private def compileBody(body: ir.Body): Seq[Atom] =
    body.atoms.map(compileAtom)

  private def compileAtom(atom: ir.Atom): Atom = atom match
    case ir.Call(RefByName(name), args, false) => Atom.Call(qualifyName(name), args.map(compileArg))
    case ir.Call(RefByName(name), args, true) => Atom.Not(Atom.Call(qualifyName(name), args.map(compileArg)))
    case ir.ExtensionalCall(RefByName(name), args, false) => Atom.Call(qualifyName(name), args.map(compileArg))
    case ir.ExtensionalCall(RefByName(name), args, true) => Atom.Not(Atom.Call(qualifyName(name), args.map(compileArg)))
    case ir.Eq(lhs, rhs, false) => Atom.Compare(compileTerm(lhs), Comparator.EQ, compileTerm(rhs))
    case ir.Eq(lhs, rhs, true) => Atom.Compare(compileTerm(lhs), Comparator.NEQ, compileTerm(rhs))
    case arith.BinCompare(lhs, rhs, c) =>
      val op = Parser.comparator.parseAll(c).toOption.get
      Atom.Compare(compileTerm(lhs), op, compileTerm(rhs))
    case data.Deconstruct(t, RefByName(name), args, false) => Atom.Compare(compileTerm(t), EQ, Term.Constr(qualifyName(name), args.map(compileArg)))
    case data.Deconstruct(t, name, args, true) => ???
    case agg.Aggregate(RefByName(name), args, op) =>
      val result = args.zipWithIndex.collect {
        case (col: AggregateColumnArg, idx) => col -> idx
      }
      val (AggregateColumnArg(ir.Var(RefByName(resultVar))), resultIdx) = result.head : @unchecked
      // TODO need to generate safely
      val aggregatorVar = ir.Var(ir.Name("aggregatorVar"))
      val replacedArgs = args.patch(resultIdx, Seq(aggregatorVar.arg), 1)
      val callArgs = replacedArgs.map {
        case TermArg(t) => compileTerm(t)
      }
      val souffleAgg = op match
        case arith.ArithmeticAggregationOperator.MinInt => Aggregator.Min(Term.Var(cleanName(aggregatorVar.name)), Seq(Atom.Call(qualifyName(name), callArgs)))
        case arith.ArithmeticAggregationOperator.MaxInt => Aggregator.Max(Term.Var(cleanName(aggregatorVar.name)), Seq(Atom.Call(qualifyName(name), callArgs)))
        case arith.ArithmeticAggregationOperator.SumInt => Aggregator.Sum(Term.Var(cleanName(aggregatorVar.name)), Seq(Atom.Call(qualifyName(name), callArgs)))
        case count@arith.ArithmeticAggregationOperator.Count => throw new IllegalArgumentException(s"Currently do not support count aggregation $count")
        case defined: AggregationOperatorUserDefined => throw new IllegalArgumentException(s"Currently do not support user-defined aggregation $defined")
      Atom.Compare(Term.Var(resultVar.name), EQ, Term.AggregatorTerm(souffleAgg))

  private def qualifyName(name: ir.Name): QualifiedName = QualifiedName(Seq(cleanName(name)))

  def cleanName(name: ir.Name): String =
    //if (name.name.startsWith(GenerateIR.WILDCARD))
    //  "_"
    //else
    name.name.replace("$", "_")

  private def compileArg(a: ir.Arg): Term = a match
    case ir.TermArg(t) => compileTerm(t)
    case ir.WildcardArg() => Term.Var("_")

  private def compileTerm(t: ir.Term): Term = t match
    case ir.Var(RefByName(name)) => Term.Var(cleanName(name))
    case ir.Cast(t, ty) => Term.TypeCast(compileTerm(t), compileType(ty))
    case arith.IntNum(n) => Term.NumberLit(n)
    case arith.DoubleNum(n) => Term.FloatLit(n.toFloat)
    case arith.BinOp(lhs, rhs, "+") => Term.Binary(compileTerm(lhs), BinOp.Add, compileTerm(rhs))
    case arith.BinOp(lhs, rhs, "-") => Term.Binary(compileTerm(lhs), BinOp.Sub, compileTerm(rhs))
    case arith.BinOp(lhs, rhs, "*") => Term.Binary(compileTerm(lhs), BinOp.Mul, compileTerm(rhs))
    case arith.BinOp(lhs, rhs, "/") => Term.Binary(compileTerm(lhs), BinOp.Div, compileTerm(rhs))
    case arith.BinOp(lhs, rhs, "%") => Term.Binary(compileTerm(lhs), BinOp.Rem, compileTerm(rhs))
    case arith.BinOp(lhs, rhs, "min") => Term.IntrinsicFunctorApp(IntrinsicFunctor.Min, Seq(compileTerm(lhs), compileTerm(rhs)))
    case arith.BinOp(lhs, rhs, "max") => Term.IntrinsicFunctorApp(IntrinsicFunctor.Max, Seq(compileTerm(lhs), compileTerm(rhs)))
    case arith.UnOp(t, "abs") => Term.IntrinsicFunctorApp(IntrinsicFunctor.Max, Seq(compileTerm(t), Term.Binary(compileTerm(t), BinOp.Mul, Term.NumberLit(-1))))
    case string.StringLit(s) => Term.StringLit(s)
    case string.StringConcat(t1, t2) => Term.IntrinsicFunctorApp(IntrinsicFunctor.Cat, Seq(compileTerm(t1), compileTerm(t2)))
    case data.Construct(RefByName(name), args) => Term.Constr(qualifyName(name), args.map(compileTerm))

  private def compileType(ty: ir.Type): Type = ty match
    case arith.TInt => Type.Number
    case arith.TDouble => Type.Float
    case string.TString => Type.Symbol
    case data.TData(RefByName(name)) => Type.Name(qualifyName(name))

