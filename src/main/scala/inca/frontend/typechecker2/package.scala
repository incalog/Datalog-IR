package inca.frontend

import inca.frontend.core.Core.{Module, Name, PatternFunction, TypeAnno}


package object typechecker2 {
  type VarCtx = Map[Name, TypeAnno]
  type FunEnv = Map[Name, PatternFunction]
  type ModuleEnv = Map[Name, Module]
}
