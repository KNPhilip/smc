package smc.optimizer

import smc.syntaxAnalyzer.StateMachine

final class Optimizer {
  def optimize(machine: StateMachine): OptimizedStateMachine = {
    val optimized = new OptimizedStateMachine(machine.name, machine.initialState)

    addStates(machine, optimized)
    addEvents(machine, optimized)
    addActions(machine, optimized)

    optimized
  }

  private def addStates(machine: StateMachine, optimized: OptimizedStateMachine): Unit =
    machine.states
      .filterNot(_.isSuperState)
      .foreach(s => optimized.states += s.name)

  private def addEvents(machine: StateMachine, optimized: OptimizedStateMachine): Unit =
    for {
      state <- machine.states
      event <- state.events
    } optimized.events += event.name

  private def addActions(machine: StateMachine, optimized: OptimizedStateMachine): Unit =
    for {
      state <- machine.states
      action <- state.entryActions ++ state.exitActions ++
        state.events.flatMap(_.actions)
    } optimized.actions += action
}
