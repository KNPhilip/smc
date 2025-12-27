package smc.semanticAnalyzer

import smc.semanticAnalyzer.SemanticError.*
import smc.syntaxAnalyzer.{StateMachineSyntax, StateMachine, State}

final class SemanticAnalyzer {
  var syntax = new SemanticSyntax()

  def analyze(input: StateMachineSyntax): SemanticSyntax = {
    syntax = new SemanticSyntax()
    checkForErrors(input)
    syntax
  }

  private def checkForErrors(input: StateMachineSyntax): Unit = {
    checkForNoMachines(input)
    checkForDuplicateMachines(input)
    input.machines.foreach(analyzeMachine)
  }

  private def checkForNoMachines(input: StateMachineSyntax): Unit =
    if (input.machines.isEmpty)
      syntax.addError(NO_MACHINES)

  private def checkForDuplicateMachines(input: StateMachineSyntax): Unit =
    if (input.machines.map(_.name).toSet.size != input.machines.size)
      syntax.addError(DUPLICATE_MACHINE)

  private def analyzeMachine(input: StateMachine): Unit = {
    checkForNoInitialState(input)
    checkForNoTransitions(input)
    checkForUndefinedInitialState(input)
    analyzeStates(input.states.toList)
  }

  private def checkForNoInitialState(input: StateMachine): Unit =
    if (input.initialState == null)
      syntax.addError(NO_INITIAL_STATE)

  private def checkForNoTransitions(input: StateMachine): Unit =
    if (input.states.isEmpty)
      syntax.addError(NO_TRANSITIONS)

  private def checkForUndefinedInitialState(input: StateMachine): Unit =
    if (!input.states.map(_.name).contains(input.initialState))
      syntax.addError(UNDEFINED_INITIAL_STATE)

  private def analyzeStates(input: List[State]): Unit = {
    checkForUndefinedNextState(input)
    checkForUnusedStates(input)
  }

  private def checkForUndefinedNextState(input: List[State]): Unit = {
    val stateNames = input.map(_.name).toSet

    val allDefined = input.forall { state =>
      state.events.forall { event =>
        event.targetState != null && stateNames.contains(event.targetState)
      }
    }

    if (!allDefined)
      syntax.addError(UNDEFINED_NEXT_STATE)
  }

  private def checkForUnusedStates(input: List[State]): Unit = {
    val isTargeted: String => Boolean =
      name => input.exists(_.events.exists(_.targetState == name))

    val unused = input
      .map(_.name)
      .filterNot(isTargeted)
      .toSet

    if (unused.nonEmpty)
      syntax.addError(UNUSED_STATE)
  }
}
