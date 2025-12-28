package smc.optimizer

import smc.syntaxAnalyzer.StateMachine

final class Optimizer {
  def optimize(machine: StateMachine): OptimizedStateMachine = {
    val optimized = new OptimizedStateMachine(machine.name, machine.initialState)

    optimized
  }
}
