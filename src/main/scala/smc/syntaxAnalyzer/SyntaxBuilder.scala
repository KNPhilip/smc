package smc.syntaxAnalyzer

final class SyntaxBuilder {
  private val syntax: StateMachineSyntax = new StateMachineSyntax()
  private var machine: StateMachine = null
  private var transition: State = null
  private var name: String = null

  def getStateMachine(): StateMachineSyntax = syntax

  def addMachine(): Unit = {
    machine = new StateMachine()
    machine.name = name
  }

  def setInitialState(): Unit = {
    machine.initialState = name
  }

  def addTransition(): Unit = {
    transition = new State()
    transition.name = name
  }

  def setEvent(): Unit = {
    transition.events += new Event()
    transition.events.last.name = name
  }

  def setEmptyNextState(): Unit = {
    transition.events.last.targetState = transition.name
  }

  def setNextState(): Unit = {
    transition.events.last.targetState = name
  }

  def addAction(): Unit = {
    transition.events.last.actions += name
  }

  def markAsSuperstate(): Unit = {
    transition.name = name
    transition.isSuperState = true
  }

  def setEntryAction(): Unit = {
    transition.entryActions += name
  }

  def setExitAction(): Unit = {
    transition.exitActions += name
  }

  def concludeTransition(): Unit = {
    machine.states += transition
    transition = null
  }

  def concludeStateMachine(): Unit = {
    syntax.machines += machine
    machine = null
    transition = null
  }

  def setName(name: String): Unit = {
    this.name = name
  }

  def syntaxError(line: Int, position: Int): Unit = {

  }

  def machineError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit = {

  }

  def transitionError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit = {

  }

  def subtransitionError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit = {

  }

  def superstateError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit = {

  }

  def entryExitError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit = {

  }
}
