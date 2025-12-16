package smc.syntaxAnalyzer

enum SyntaxEvent:
  case Machine
  case Initial
  case State
  case Event
  case Superstate
  case Inherits
  case Entry
  case Exit
  case Arrow
  case OpenBrace
  case ClosedBrace
  case Dash
  case Name
