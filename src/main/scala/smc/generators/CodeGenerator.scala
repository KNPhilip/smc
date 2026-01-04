package smc.generators

import smc.optimizer.OptimizedStateMachine

trait CodeGenerator {
  def generate(machine: OptimizedStateMachine): Unit
}
