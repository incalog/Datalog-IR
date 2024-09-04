package inca.ir.analysis.base.effect

import sturdy.effect.failure.FailureKind
import sturdy.values.Finite

enum Failure extends FailureKind:
  // BaseAbstractInterpreter
  case ProgramFailure

  // Terms
  case UnknownTerm

  // Arg
  case UnknownArg

  // Atoms
  case UnknownAtom
  case InvalidBindings

  case TypeError
  case RefNotFound

given IRFailure: Finite[Failure] with {}