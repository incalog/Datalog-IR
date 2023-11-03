package inca.souffle.compile
import inca.ir
import inca.ir.extension.string
import inca.ir.extension.arithmetic as arith
import inca.ir.extension.data
import inca.souffle.syntax.*

// TODO what is output? Need main hint
// Core + Arithmetic + String + Data?
object GenerateSouffle:
  def compileModule(module: ir.Module): Program =
    val illegalFeatures = module.lang.features -- Set(inca.ir.BaseIR, inca.ir.extension.arithmetic.IR, inca.ir.extension.string.IR, inca.ir.extension.data.IR)
    if (illegalFeatures.nonEmpty)
      throw IllegalArgumentException(s"Cannot compile module containing the following features: ${illegalFeatures.mkString(", ")}")

    val contents = module.contents.flatMap {
      case ir.Relation(name, params, bodies) =>
        val attrs = params.map { p =>
          Attribute(p.name.name, compileType(p.ty))
        }
        val relDecl = ProgramContent.RelationDecl(Seq(name.name), attrs, Seq(), None)
        val head = Atom.Call(QualifiedName(Seq(name.name)), params.map(p => Term.Var(p.name.name)))

        val conjunctions = bodies.map(compileBody)
        val rules = conjunctions.map { conjunction => ProgramContent.Rule(Seq(head), conjunction.atoms, None) }
        // TODO output directive?
        Seq(relDecl) ++ rules

      case ir.ExtensionalRelation(name, params) =>
        val attrs = params.map { p =>
          Attribute(p.name.name, compileType(p.ty))
        }
        val relDecl = ProgramContent.RelationDecl(Seq(name.name), attrs, Seq(), None)
        // TODO
        val inputDirective = ProgramContent.Directive
        Seq(relDecl, inputDirective)
      case data.DataDefinition(name, cases) =>
        val adtBranches = cases.map {
          case data.CaseDefinition(name, args) =>

            val cotrArgs = args.zipWithIndex.map { case(ty, idx) =>
              Attribute(s"param_$idx", compileType(ty))
            }
            ADTConstructor(name.name, cotrArgs)
        }
        val adtDef = TypeDeclConstraint.EqTypeAlternativeADTBranches(adtBranches)
        val typeDecl = ProgramContent.TypeDecl(name.name, adtDef)
        Seq(typeDecl)
    }
    Program(contents)

  def compileBody(body: ir.Body): Conjunction =
    Conjunction(body.atoms.map(compileAtom))

  def compileAtom(atom: ir.Atom): Atom = atom match
    case ir.Call(name, args) => Atom.Call(qualifyName(name), args.map(compileTerm))
    case ir.NegCall(name, args) => Atom.Not(Atom.Call(qualifyName(name), args.map(compileTerm)))
    case ir.ExtensionalCall(name, args) => Atom.Call(qualifyName(name), args.map(compileTerm))
    case ir.NegExtensionalCall(name, args) => Atom.Not(Atom.Call(qualifyName(name), args.map(compileTerm)))
    case ir.Eq(lhs, rhs) => Atom.Equal(compileTerm(lhs), compileTerm(rhs))
    case ir.Neq(lhs, rhs) => Atom.Unequal(compileTerm(lhs), compileTerm(rhs))
    case arith.BinCompare(lhs, rhs, "<") => Atom.LessThan(compileTerm(lhs), compileTerm(rhs))
    case arith.BinCompare(lhs, rhs, "<=") => Atom.LessThanEqual(compileTerm(lhs), compileTerm(rhs))
    case arith.BinCompare(lhs, rhs, ">") => Atom.GreaterThan(compileTerm(lhs), compileTerm(rhs))
    case arith.BinCompare(lhs, rhs, ">=") => Atom.GreaterThanEqual(compileTerm(lhs), compileTerm(rhs))
    case data.Deconstruct(t, name, args) => Atom.Equal(compileTerm(t), Term.Constr(name.name, args.map(compileTerm)))

  def qualifyName(n: ir.Name): QualifiedName = QualifiedName(Seq(n.name))

  def compileTerm(t: ir.Term): Term = t match
    case ir.Var(name) => Term.Var(name.name)
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
    case arith.UnOp(t, "abs") =>
      Term.IntrinsicFunctorApp(IntrinsicFunctor.Max, Seq(compileTerm(t), Term.Binary(compileTerm(t), BinOp.Mul, Term.NumberLit(-1))))
    case string.StringLit(s) => Term.StringLit(s)
    case string.StringConcat(t1, t2) => Term.IntrinsicFunctorApp(IntrinsicFunctor.Cat, Seq(compileTerm(t1), compileTerm(t2)))
    case data.Construct(name, args) => Term.Constr(name.name, args.map(compileTerm))

  def compileType(ty: ir.Type): Type = ty match
    case arith.TInt => Type.Number
    case arith.TDouble => Type.Float
    case string.TString => Type.Symbol
    case data.TData(name) => Type.Name(qualifyName(name))

