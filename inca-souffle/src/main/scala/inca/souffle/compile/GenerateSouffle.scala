package inca.souffle.compile
import inca.ir
import inca.ir.TermArg
import inca.ir.extension.string
import inca.ir.extension.arithmetic as arith
import inca.ir.extension.aggregate as agg
import inca.ir.extension.aggregate.{AggregateColumnArg, AggregationOperatorBuiltIn, AggregationOperatorUserDefined}
import inca.ir.extension.data
import inca.souffle.syntax.*

// TODO what is output? Need main hint
// Core + Arithmetic + String + Data
object GenerateSouffle:

  def compileModule(module: ir.Module): Program =
    val compilableFeatures = Set(ir.BaseIR, arith.IR, string.IR, data.IR)
    val illegalFeatures = module.lang.features -- compilableFeatures
    if (illegalFeatures.nonEmpty)
      println(s"[WARNING]: Cannot compile module containing the following features: ${illegalFeatures.mkString(", ")}")
      //throw IllegalArgumentException(s"Cannot compile module containing the following features: ${illegalFeatures.mkString(", ")}")

    val contents = module.contents.flatMap {
      case ir.Relation(name, params, bodies) =>
        val attrs = params.map { p =>
          Attribute(cleanName(p.name), compileType(p.ty))
        }
        val relDecl = ProgramContent.RelationDecl(Seq(cleanName(name)), attrs, Seq(), None)
        val head = Atom.Call(QualifiedName(Seq(cleanName(name))), params.map(p => Term.Var(cleanName(p.name))))

        val conjunctions = bodies.map(compileBody)
        val rules = conjunctions.map { conjunction => ProgramContent.Rule(Seq(head), conjunction.atoms, None) }
        // TODO just a small hack for output directive
        val outputDirective = ProgramContent.Directive(DirectiveQualifier.Output, qualifyName(name), Map())
        Seq(relDecl, outputDirective) ++ rules

      case ir.ExtensionalRelation(name, params) =>
        val attrs = params.map { p =>
          Attribute(cleanName(p.name), compileType(p.ty))
        }
        val relDecl = ProgramContent.RelationDecl(Seq(cleanName(name)), attrs, Seq(), None)
        val inputDirective = ProgramContent.Directive(DirectiveQualifier.Input, qualifyName(name), Map())
        Seq(relDecl, inputDirective)
      case data.DataDefinition(name, cases) =>
        val adtBranches = cases.map {
          case data.CaseDefinition(name, args) =>
            val cotrArgs = args.zipWithIndex.map { case(ty, idx) =>
              Attribute(s"param_$idx", compileType(ty))
            }
            ADTConstructor(cleanName(name), cotrArgs)
        }
        val adtDef = TypeDeclConstraint.ADTType(adtBranches)
        val typeDecl = ProgramContent.TypeDecl(cleanName(name), adtDef)
        Seq(typeDecl)
    }
    Program(contents)

  private def compileBody(body: ir.Body): Conjunction =
    Conjunction(body.atoms.map(compileAtom))

  private def compileAtom(atom: ir.Atom): Atom = atom match
    case ir.Call(name, args) => Atom.Call(qualifyName(name), args.map(compileArg))
    case ir.NegCall(name, args) => Atom.Not(Atom.Call(qualifyName(name), args.map(compileArg)))
    case ir.ExtensionalCall(name, args) => Atom.Call(qualifyName(name), args.map(compileArg))
    case ir.NegExtensionalCall(name, args) => Atom.Not(Atom.Call(qualifyName(name), args.map(compileArg)))
    case ir.Eq(lhs, rhs) => Atom.Equal(compileTerm(lhs), compileTerm(rhs))
    case ir.Neq(lhs, rhs) => Atom.Unequal(compileTerm(lhs), compileTerm(rhs))
    case arith.BinCompare(lhs, rhs, "<") => Atom.LessThan(compileTerm(lhs), compileTerm(rhs))
    case arith.BinCompare(lhs, rhs, "<=") => Atom.LessThanEqual(compileTerm(lhs), compileTerm(rhs))
    case arith.BinCompare(lhs, rhs, ">") => Atom.GreaterThan(compileTerm(lhs), compileTerm(rhs))
    case arith.BinCompare(lhs, rhs, ">=") => Atom.GreaterThanEqual(compileTerm(lhs), compileTerm(rhs))
    case data.Deconstruct(t, name, args) => Atom.Equal(compileTerm(t), Term.Constr(cleanName(name), args.map(compileArg)))
    case agg.Aggregate(name, args, op) =>
      val result = args.zipWithIndex.collect {
        case (col: AggregateColumnArg, idx) => col -> idx
      }
      val (AggregateColumnArg(ir.Var(resultVar)), resultIdx) = result.head
      // TODO need to generate safely
      val aggregatorVar = ir.Var(ir.Name("aggregatorVar"))
      val replacedArgs = args.patch(resultIdx, Seq(aggregatorVar.arg), 1)
      val callArgs = replacedArgs.map {
        case TermArg(t) => compileTerm(t)
      }
      val souffleAgg = op match
        case arith.ArithmeticAggregationOperator.Min => Aggregator.Min(Term.Var(cleanName(aggregatorVar.name)), Seq(Atom.Call(qualifyName(name), callArgs)))
        case arith.ArithmeticAggregationOperator.Max => Aggregator.Max(Term.Var(cleanName(aggregatorVar.name)), Seq(Atom.Call(qualifyName(name), callArgs)))
        case arith.ArithmeticAggregationOperator.Sum => Aggregator.Sum(Term.Var(cleanName(aggregatorVar.name)), Seq(Atom.Call(qualifyName(name), callArgs)))
        case count@arith.ArithmeticAggregationOperator.Count => throw new IllegalArgumentException(s"Currently do not support count aggregation $count")
        case defined: AggregationOperatorUserDefined => throw new IllegalArgumentException(s"Currently do not support user-defined aggregation $defined")
      Atom.Equal(Term.Var(resultVar.name), Term.AggregatorTerm(souffleAgg))

  private def qualifyName(name: ir.Name): QualifiedName = QualifiedName(Seq(cleanName(name)))

  def cleanName(name: ir.Name): String = name.name.replace("$", "_")

  private def compileArg(a: ir.Arg): Term = a match
    case ir.TermArg(t) => compileTerm(t)
    case ir.WildcardArg => Term.Var("_")

  private def compileTerm(t: ir.Term): Term = t match
    case ir.Var(name) => Term.Var(cleanName(name))
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
    case data.Construct(name, args) => Term.Constr(cleanName(name), args.map(compileTerm))

  private def compileType(ty: ir.Type): Type = ty match
    case arith.TInt => Type.Number
    case arith.TDouble => Type.Float
    case string.TString => Type.Symbol
    case data.TData(name) => Type.Name(qualifyName(name))

