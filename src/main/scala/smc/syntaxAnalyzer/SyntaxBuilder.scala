package smc.syntaxAnalyzer

trait SyntaxBuilder {
  def addMachine(): Unit
  def setInitialState(): Unit
  def addTransition(): Unit
  def setEvent(): Unit
  def setEmptyNextState(): Unit
  def setNextState(): Unit
  def addAction(): Unit
  def setActionAndConclude(): Unit
  def markAsSuperstate(): Unit
  def markAsSubtransition(): Unit
  def setEntryAction(): Unit
  def setExitAction(): Unit
  def concludeTransition(): Unit
  def concludeStateMachine(): Unit
  def setName(name: String): Unit
  def syntaxError(line: Int, position: Int): Unit
  def machineError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit
  def transitionError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit
  def subtransitionError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit
  def superstateError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit
  def entryExitError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit
}
