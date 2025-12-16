package smc.syntaxAnalyzer

enum SyntaxState:
  case Machine
  case MachineNamed
  case InitialArrow
  case InitialArrowNamed
  case MachineSpec
  case MachineValue
