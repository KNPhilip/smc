package smc

import munit.FunSuite

import scala.collection.mutable.ListBuffer
import smc.optimizer.{Optimizer, OptimizedStateMachine}
import smc.syntaxAnalyzer.{StateMachineSyntax, StateMachine}

class OptimizationSuite extends FunSuite {
  protected var syntax = new StateMachineSyntax()
  private val optimizer = new Optimizer()

  override def beforeEach(context: BeforeEach): Unit =
    syntax = new StateMachineSyntax()

  protected def optimize(): ListBuffer[OptimizedStateMachine] =
    syntax.machines.map(optimizer.optimize)
}

class BasicOptimizerSuite extends OptimizationSuite {
  test("Name of machine") {
    syntax.machines += new StateMachine("myMachine")
    val machines = optimize()
    assertEquals("myMachine", "myMachine")
  }
}
