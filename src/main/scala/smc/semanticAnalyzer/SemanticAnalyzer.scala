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
  }

  private def checkForNoInitialState(input: StateMachine): Unit =
    if (input.initialState == null)
      syntax.addError(NO_INITIAL_STATE)

  private def checkForNoTransitions(input: StateMachine): Unit =
    if (input.states.isEmpty)
      syntax.addError(NO_TRANSITIONS)

  private def checkForUndefinedInitialState(input: StateMachine): Unit =
    if (!input.states.contains(input.initialState))
      syntax.addError(UNDEFINED_INITIAL_STATE)
}
