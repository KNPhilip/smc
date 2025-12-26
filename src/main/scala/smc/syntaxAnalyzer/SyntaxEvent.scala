package smc.syntaxAnalyzer

enum SyntaxEvent:
  case
  Machine,
  Initial,
  State,
  Event,
  Superstate,
  Inherits,
  Entry,
  Exit,
  Arrow,
  OpenBrace,
  ClosedBrace,
  Dash,
  Name
