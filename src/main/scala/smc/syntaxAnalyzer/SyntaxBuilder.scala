package smc.syntaxAnalyzer

import scala.collection.mutable.ListBuffer
import smc.syntaxAnalyzer.ErrorType.*

final class SyntaxBuilder {
  private val syntax: StateMachineSyntax = new StateMachineSyntax()
  private var machine: StateMachine = null
  private var transition: State = null
  private var name: String = null

  def getStateMachine: StateMachineSyntax = syntax

  def clear(): Unit = {
    syntax.machines = ListBuffer.empty
    syntax.errors = ListBuffer.empty
    machine = null
    transition = null
    name = null
  }

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
    transition.isAbstract = true
  }

  def addEntryAction(): Unit =
    transition.entryActions += name

  def addExitAction(): Unit =
    transition.exitActions += name

  def addInheritance(): Unit =
    transition.superStates += name

  def concludeTransition(): Unit = {
    machine.states += transition
    transition = null
  }

  def concludeStateMachine(): Unit = {
    if (transition != null)
      machine.states += transition

    syntax.machines += machine
    machine = null
    transition = null
  }

  def setName(name: String): Unit =
    this.name = name

  def syntaxError(line: Int, position: Int): Unit =
    syntax.errors.addOne(new SyntaxError(SyntaxError, "", line, position))

  def tableError(errorType: ErrorType, state: SyntaxState,
                 event: SyntaxEvent, line: Int, position: Int): Unit =
    syntax.errors.addOne(new SyntaxError(errorType, s"$state|$event", line, position))
}
