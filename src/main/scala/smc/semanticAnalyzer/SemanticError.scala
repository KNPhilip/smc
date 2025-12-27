package smc.semanticAnalyzer

enum SemanticError:
  case
  NO_MACHINES,
  DUPLICATE_MACHINE,
  NO_INITIAL_STATE,
  NO_TRANSITIONS,
  UNDEFINED_INITIAL_STATE
