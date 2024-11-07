package inca.ir.analysis.base.effect

import sturdy.effect.failure.FailureKind
import sturdy.values.Finite

trait BaseIRFailure extends FailureKind
  
// BaseGenericInterpreter
case object ProgramFailure extends BaseIRFailure
  
// Terms
case object UnknownTerm extends BaseIRFailure
case object UnresolvedVariable extends BaseIRFailure

// Relation
case object NoParamRelation extends BaseIRFailure

// Arg
case object UnknownArg extends BaseIRFailure

// Atoms
//case FailedComparison
case object UnknownAtom extends BaseIRFailure
case object InvalidBindings extends BaseIRFailure
case object RefNotFound extends BaseIRFailure

// relation ops
case object ColumnMismatch extends BaseIRFailure

given IRFailure: Finite[BaseIRFailure] with {}