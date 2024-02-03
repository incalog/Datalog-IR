package inca.ir.extension.edbdata

import inca.ir.string2name
import inca.ir.extension.arithmetic.TInt
import inca.ir.extension.string.TString

val edbExp = Seq(
  EdbNodeDefinition("Exp"),
  EdbNodeDefinition("Var", "Exp"),
  EdbFieldDefinition("Var", "name", TEdbValue(TString)),
  EdbNodeDefinition("Num", "Exp"),
  EdbFieldDefinition("Num", "value", TEdbValue(TInt)),
  EdbNodeDefinition("Add", "Exp"),
  EdbFieldDefinition("Add", "lhs", TEdbNode("Exp")),
  EdbFieldDefinition("Add", "rhs", TEdbNode("Exp")),
  EdbNodeDefinition("Let", "Exp"),
  EdbFieldDefinition("Let", "name", TEdbValue(TString)),
  EdbFieldDefinition("Let", "bound", TEdbNode("Exp")),
  EdbFieldDefinition("Let", "body", TEdbNode("Exp"))
)