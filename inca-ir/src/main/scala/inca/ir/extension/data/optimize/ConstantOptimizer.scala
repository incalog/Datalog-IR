package inca.ir.extension.data.optimize

import inca.ir
import inca.ir.analysis.base.values.Value
import inca.ir.extension.data.{Construct, Deconstruct}
import inca.ir.extension.data.analysis.interpreter.ConstantDataV
import sturdy.values.{Top, Topped}
import inca.ir.*
import inca.ir.optimize.ConstantBaseIROptimizer
import inca.ir.optimize.{isTrue, isFalse}

trait ConstantOptimizer extends ConstantBaseIROptimizer:

  override def mayEliminate(t: Term): Boolean = t match
    case Construct(_, args) => isConstant(t) && args.forall(mayEliminate)
    case _ => super.mayEliminate(t)
  
  override def valueToTermInternal(value: Value): Option[Term] = value match
    case ConstantDataV(caseDef, args) =>
      val argsV = args.flatMap(valueToTermInternal)
      if (argsV.size != args.size)
        None
      else
        Some(Construct(caseDef.name, argsV))
    case _ => super.valueToTermInternal(value)

  def deconstructBindings(args: Seq[Arg], cargs: Seq[Value]): Seq[Topped[Boolean]] = args.zip(cargs).map {
    case (WildcardArg(), v) => Topped.Actual(true)
    case (TermArg(x: Var), v) if isParam(x.ref) => Topped.Top
    case (TermArg(t), v) => getTermResult(t).headOption match
      case None => Topped.Top
      case Some(v0) => eqOps.equ(v0, v)
  }

  override def visitAtom(atom: Atom): Seq[Atom] = atom match
    case Deconstruct(t, caseRef, args, false) =>
      getTermResult(t).headOption match
        case Some(ConstantDataV(caseDef, cargs)) if caseDef == caseRef.target.get =>
          val bindings = deconstructBindings(args, cargs)
          if (bindings.exists(_.isFalse)) {
            logOptimizationStat("constant failed deconstruct", 1, _+1)
            throw FailedBody
          } else if (bindings.forall(_.isTrue)) {
            logOptimizationStat("constant deconstruct", 1, _+1)
            Seq()
          } else
            super.visitAtom(atom)
        case Some(Value.Top) =>
          super.visitAtom(atom)
        case Some(_) =>
          logOptimizationStat("constant failed deconstruct", 1, _+1)
          throw FailedBody
        case _ => super.visitAtom(atom)
    case Deconstruct(t, caseRef, args, true) => getTermResult(t).headOption match
      case Some(ConstantDataV(caseDef, cargs)) if caseDef == caseRef.target.get =>
        val bindings = deconstructBindings(args, cargs)
        if (bindings.exists(_.isFalse)) {
          logOptimizationStat("constant deconstruct", 1, _+1)
          Seq()
        } else if (bindings.forall(_.isTrue)) {
          logOptimizationStat("constant failed deconstruct", 1, _+1)
          throw FailedBody
        } else
          super.visitAtom(atom)
      case _ => super.visitAtom(atom)
    case _ => super.visitAtom(atom)

