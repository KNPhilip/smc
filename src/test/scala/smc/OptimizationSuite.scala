package smc

import munit.FunSuite
import scala.collection.mutable.ListBuffer
import smc.optimizer.{Optimizer, OptimizedStateMachine}
import smc.syntaxAnalyzer.{StateMachineSyntax, StateMachine, State, Event}

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
    assertEquals(machines.head.name, "myMachine")
  }

  test("Name of initial state") {
    syntax.machines += new StateMachine("") {
      initialState = "initial"
    }
    val machines = optimize()
    assertEquals(machines.head.initialState, "initial")
  }

  test("Multiple machines") {
    syntax.machines += new StateMachine("hello") {
      initialState = "world"
    }
    syntax.machines += new StateMachine("foo") {
      initialState = "bar"
    }

    val machines = optimize()
    assertEquals(machines.head.name, "hello")
    assertEquals(machines.head.initialState, "world")
    assertEquals(machines.last.name, "foo")
    assertEquals(machines.last.initialState, "bar")
  }

  test("All states collected") {
    syntax.machines += new StateMachine("") {
      states += new State("state1")
      states += new State("state2")
      states += new State("state3")
    }

    val machines = optimize()
    assertEquals(machines.head.states,
      ListBuffer("state1", "state2", "state3"))
  }

  test("Superstates are ignored") {
    syntax.machines += new StateMachine("") {
      states += new State("state1")
      states += new State("state2") {
        isSuperState = true
      }
      states += new State("state3") {
        isSuperState = true
      }
    }

    val machines = optimize()
    assertEquals(machines.head.states, ListBuffer("state1"))
  }

  test("All events collected") {
    syntax.machines += new StateMachine("") {
      states += new State("state1") {
        events += new Event("event1")
        events += new Event("event2")
      }
      states += new State("state2") {
        events += new Event("event3")
        events += new Event("event4")
      }
    }

    val machines = optimize()
    assertEquals(machines.head.events,
      ListBuffer("event1", "event2", "event3", "event4"))
  }

  test("All actions collected") {
    syntax.machines += new StateMachine("") {
      states += new State("state1") {
        events += new Event("event1") {
          actions += "action1"
          actions += "action2"
        }
        events += new Event("event2") {
          actions += "action3"
        }
      }
      states += new State("state2") {
        events += new Event("event3") {
          actions += "action4"
        }
        events += new Event("event4") {
          actions += "action5"
          actions += "action6"
        }
      }
    }

    val machines = optimize()
    assertEquals(machines.head.actions,
      ListBuffer("action1", "action2", "action3", "action4", "action5", "action6"))
  }
}
