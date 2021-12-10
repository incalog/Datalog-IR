package inca.lattice.verification

import inca.frontend.functional.core.{Call, Let, Match, _}
import inca.lattice.verification.z3._

object Verifier {
  // Idee: Verifier bekommt ein Modul, findet die
  //  annotierten join Funktionen, speichert die markierten Eigenschaften und gibt die
  //  Funktion weiter an compileFunction. Das Ergebnis wird dann in einer getrennten Methode
  //  mit den Assertions der jeweiligen Eigenschaft ergänzt und getestet.
  //  Zuerst mal ohne unjoin
  def transModule(module: Module): Seq[Script] = {
    var joinFunctions = Seq()
    module.content.foreach {
      case f@FunctionDef(annos, vis, name, params, outType, body) => annos.foreach{
        case JoinFunctionAnno(props) => {
          joinFunctions :+= compileFun(f)
          // TODO props überprüfen
        }
      }
      case DataDef(annos, vis, name, constrs) =>
    }
    joinFunctions
  }

  def compileFun(functionDef: FunctionDef): Script = {
    //case class FunctionDef(annos: Seq[Annotation], vis: Option[Visibility],
    //  name: Name, params: Seq[Param], outType: Type, body: Expression)
    // TODO: Parameter Map erstellen und mitgeben? Der ReturnType sollte auch weitergegeben werden
    //  Name, visibility, annotations sind hier egal
      compileExpr(functionDef.body)
  }

  def compileExpr(body: Expression): Script = {
    body match {
      case NoneExp() =>
      case SetComprehension(build, predicates) =>
      case BaseApplyInfix(left, op, right) =>
      case SetExp(es) =>
      case Var(name) =>
      case SomeExp(e) =>
      case Tuple(exps) =>
      case BaseLit(code) =>
      case Lambda(vs, body) =>
      case If(cnd, thn, els) =>
      case BaseApply(fun, args) =>
      case Match(matchee, cases) =>
      case SetMember(tup, set, neg) =>
      case Call(fun, args, transitive) =>
      case SetFold(anno, init, op, set) =>
      case Let(names, anno, bound, body) =>
    }
    Script(Seq())
  }
}
