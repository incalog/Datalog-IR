package inca.ir.analysis.base.effect

import sturdy.effect.failure.FailureKind
import sturdy.values.Finite

enum Failure extends FailureKind:

  // BaseAbstractInterpreter
  case ProgramFailure
  case TypeError
  case RefNotFound
  case MaybeEmptyCall

  // RelationOps
  case AntiJoinError
  case UnionError
  case EquiJoinError
  case EmptyVariable
  case MaybeEquiJoinError
  case MaybeFilterError
  case RenameError

given IRFailure: Finite[Failure] with {}