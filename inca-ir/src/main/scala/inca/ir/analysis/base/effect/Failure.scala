package inca.ir.analysis.base.effect

import sturdy.effect.failure.FailureKind
import sturdy.values.Finite

enum Failure extends FailureKind:
  // BaseGenericInterpreter
  case ProgramFailure

  // Terms
  case UnknownTerm

  // Arg
  case UnknownArg

  // Atoms
  case UnknownAtom
  case InvalidBindings
  case RefNotFound

  // relation ops
  case ColumnMismatch

given IRFailure: Finite[Failure] with {}