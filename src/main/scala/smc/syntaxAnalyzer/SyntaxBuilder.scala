package smc.syntaxAnalyzer

trait SyntaxBuilder {
  def addMachine(): Unit
  def setInitialState(): Unit
  def addTransition(): Unit
  def setEvent(): Unit
  def setEmptyNextState(): Unit
  def setNextState(): Unit
  def addAction(): Unit
  def markAsSuperstate(): Unit
  def markAsSubtransition(): Unit
  def setEntryAction(): Unit
  def setExitAction(): Unit
  def concludeTransition(): Unit
  def concludeStateMachine(): Unit
}
