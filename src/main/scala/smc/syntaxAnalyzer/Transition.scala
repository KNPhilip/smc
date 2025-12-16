package smc.syntaxAnalyzer

final case class Transition(
  currentState: SyntaxState,
  event: SyntaxEvent,
  newState: SyntaxState,
  action: Option[SyntaxBuilder => Unit]
)
