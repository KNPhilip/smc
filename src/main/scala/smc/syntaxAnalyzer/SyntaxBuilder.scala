package smc.syntaxAnalyzer

import smc.syntaxAnalyzer.ErrorType.*

final class SyntaxBuilder {
  private val syntax: StateMachineSyntax = new StateMachineSyntax()
  private var machine: StateMachine = null
  private var transition: State = null
  private var name: String = null

  def getStateMachine: StateMachineSyntax = syntax

  def addMachine(): Unit =
    machine = new StateMachine(name)

  def setInitialState(): Unit =
    machine.initialState = name

  def addTransition(): Unit =
    transition = new State(name)

  def setEvent(): Unit =
    transition.events += new Event(name)

  def setEmptyNextState(): Unit =
    transition.events.last.targetState = transition.name

  def setNextState(): Unit =
    transition.events.last.targetState = name

  def addAction(): Unit =
    transition.events.last.actions += name

  def markAsSuperstate(): Unit = {
    transition.name = name
    transition.isSuperState = true
  }

  def setEntryAction(): Unit =
    transition.entryActions += name

  def setExitAction(): Unit =
    transition.exitActions += name

  def concludeTransition(): Unit = {
    machine.states += transition
    transition = null
  }

  def concludeStateMachine(): Unit = {
    syntax.machines += machine
    machine = null
    transition = null
  }

  def setName(name: String): Unit =
    this.name = name

  def syntaxError(line: Int, position: Int): Unit =
    syntax.errors.addOne(new SyntaxError(SyntaxError, "", line, position))

  def machineError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit =
    syntax.errors.addOne(new SyntaxError(MachineError, s"$state|$event", line, position))

  def transitionError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit =
    syntax.errors.addOne(new SyntaxError(TransitionError, s"$state|$event", line, position))

  def subtransitionError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit =
    syntax.errors.addOne(new SyntaxError(SubtransitionError, s"$state|$event", line, position))

  def superstateError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit =
    syntax.errors.addOne(new SyntaxError(SuperstateError, s"$state|$event", line, position))

  def entryExitError(state: SyntaxState, event: SyntaxEvent, line: Int, position: Int): Unit =
    syntax.errors.addOne(new SyntaxError(EntryExitError, s"$state|$event", line, position))
}
