package smc.syntaxAnalyzer

enum SyntaxEvent:
  case
  Machine,
  Initial,
  State,
  Event,
  Abstract,
  Colon,
  Entry,
  Exit,
  Arrow,
  OpenBrace,
  ClosedBrace,
  Dash,
  Name
