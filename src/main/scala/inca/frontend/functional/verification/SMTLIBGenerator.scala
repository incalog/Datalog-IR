package inca.frontend.functional.verification

import inca.frontend.functional.core._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
import smtlib.trees.{Terms, Tree}


object SMTLIBGenerator {
  def generateFun(name: String, functionDef: FunctionDef): List[Command] = {
    //case class FunctionDef(annos: Seq[Annotation], vis: Option[Visibility],
    //  name: Name, params: Seq[Param], outType: Type, body: Expression)
    // TODO: Parameter Map erstellen und mitgeben? Der ReturnType sollte auch weitergegeben werden
    //  Name, visibility, annotations sind hier egal
    generateExpr(functionDef.body).asInstanceOf[List[Command]]
  }


  // TODO ist das okay, den returntype so zu wählen und dann so oft zu casten?
  def generateExpr(body: Expression): List[Tree] = {
    body match {
      // Hoffentlich umsetzbar
      case Var(name) =>
        Identifier(smtlib.trees.Terms.SSymbol(name.name))
      case inca.frontend.functional.core.Let(names, anno, bound, body) =>
        val varNames: Seq[SSymbol] = names.map(name => SSymbol(name.name))
        val boundTerms: Seq[Term] = generateExpr(bound).asInstanceOf[List[Term]]
        val firstBinding = VarBinding(varNames.head, boundTerms.head)
        val otherBindings = varNames.zip(boundTerms).map(x => VarBinding(x._1, x._2))
        Terms.Let(firstBinding, otherBindings, generateExpr(body).head.asInstanceOf[Term])
      case BaseApplyInfix(left, op, right) =>
        val operator: String = op.tree.value
        val funArgs: Seq[Term] = Seq(left, right).map(generateExpr).asInstanceOf[List[Term]]
        FunctionApplication(QualifiedIdentifier(Identifier(SSymbol(operator))), funArgs)
      case BaseLit(code) =>
        code.tree match {
          case scala.meta.Lit(value) => value match {
            case b: Boolean => SSymbol(b.toString)
            case by: Byte => SNumeral(by)
            case ch: Char => SSymbol(ch.toString)
            case d: Double => SDecimal(d)
            case f: Float => SDecimal(f)
            case i: Int => SNumeral(i)
            case _ => throw new Exception("Seems to be a literal, yet none of the above...?")
          }
          case _ => throw new Exception("Literal is not a literal")
        }
      case If(cnd, thn, els) => FunctionApplication(
        QualifiedIdentifier(Identifier(SSymbol("ite"))),
        Seq(cnd, thn, els).flatMap(generateExpr).asInstanceOf[Seq[Term]]
        )
      case Match(matchee: Expression, cases: Seq[(Pattern, Expression)]) =>
        val scrut = generateExpr(matchee).asInstanceOf[List[Term]].head
        val transCases = cases.flatMap(
          cas => cas._1 match {
            case ConstructorPattern(constr, args) => ???
            // egal weil Some und None eh nicht verwendet werden?
            case NonePattern() => ???
            case SomePattern(arg) => ???
          }
        )
        smtlib.extensions.tip.Terms.Match(scrut, transCases)
      case Call(fun, args, transitive) =>

        // Vielleicht nicht so leicht
      case BaseApply(fun, args) =>
        // Das sieht sehr anstrengend aus... Vielleicht irgendwie so:
        fun.tree match {
          case f: scala.meta.Term.Function =>
            val funName: String = freshName()
            val funTerm: DefineFun = generateScalaMetaFunction(funName, f)
            val funArgs: Seq[Term] = args.flatMap(generateExpr).asInstanceOf[Seq[Term]]
            val appl: FunctionApplication = FunctionApplication(
              QualifiedIdentifier(Identifier(SSymbol(funName))),
              funArgs
            )
            Seq(funTerm, appl)
          case _ => throw new Exception("Function is not a function")
        }
      case Tuple(exps) =>
      case SetExp(es) =>
      case SetComprehension(build, predicates) =>
      case SetMember(tup, set, neg) =>
      case Lambda(vs, body) =>
      case SetFold(anno, init, op, set) => //nicht verwendet

      // Sowieso nicht verwendet
      case SomeExp(e) =>
      case NoneExp() =>
    }
    List()
  }

  def generateScalaMetaFunction(funName: String, f: scala.meta.Term.Function): DefineFun = ???

  def freshName(): String = ???
}
