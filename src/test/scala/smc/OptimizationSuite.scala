package smc

import munit.FunSuite

import scala.collection.mutable.ListBuffer
import smc.optimizer.{OptimizedStateMachine, OptimizedTransition, Optimizer}
import smc.syntaxAnalyzer.{Event, State, StateMachine, StateMachineSyntax}

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
    syntax.machines += new StateMachine("machine") {
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
    syntax.machines += new StateMachine("machine") {
      states += new State("state1")
      states += new State("state2")
      states += new State("state3")
    }

    val machines = optimize()
    assertEquals(machines.head.states,
      ListBuffer("state1", "state2", "state3"))
  }

  test("Superstates are ignored") {
    syntax.machines += new StateMachine("machine") {
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
    syntax.machines += new StateMachine("machine") {
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
    syntax.machines += new StateMachine("machine") {
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

class EntryAndExitTransitionOptimizerSuite extends OptimizationSuite {
  override def beforeEach(context: BeforeEach): Unit =
    syntax = new StateMachineSyntax() {
      machines += new StateMachine("machine")
    }

  private def assertTransition(input: OptimizedTransition, name: String, event: String,
                               nextState: String, actions: ListBuffer[String]): Unit = {
    assertEquals(input.currentState, name)
    assertEquals(input.subTransitions.head.event, event)
    assertEquals(input.subTransitions.head.nextState, nextState)
    assertEquals(input.subTransitions.head.actions, actions)
  }

  test("Simple transition collected") {
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "state"
      }
    }

    val machines = optimize()
    assertEquals(machines.head.transitions.size, 1)
  }

  test("Entry actions properly collected") {
    syntax.machines.last.states += new State("state1") {
      events += new Event("event1") {
        targetState = "state2"
        actions += "action1"
      }
    }
    syntax.machines.last.states += new State("state2") {
      entryActions += "action2"
      entryActions += "action3"
      events += new Event("event2") {
        targetState = "state1"
      }
    }

    val machines = optimize()
    assertTransition(machines.head.transitions.head,
      "state1", "event1", "state2", ListBuffer("action2", "action3", "action1"))
    assertTransition(machines.head.transitions.last,
      "state2", "event2", "state1", ListBuffer.empty)
  }

  test("Exit actions properly collected") {
    syntax.machines.last.states += new State("state1") {
      events += new Event("event1") {
        targetState = "state2"
        actions += "action1"
      }
    }
    syntax.machines.last.states += new State("state2") {
      exitActions += "action2"
      exitActions += "action3"
      events += new Event("event2") {
        targetState = "state1"
        actions += "action4"
      }
    }

    val machines = optimize()
    assertTransition(machines.head.transitions.head,
      "state1", "event1", "state2", ListBuffer("action1"))
    assertTransition(machines.head.transitions.last,
      "state2", "event2", "state1", ListBuffer("action2", "action3", "action4"))
  }

  test("Superstate entry and exit actions properly collected") {
    syntax.machines.last.states += new State("exitSuper") {
      isSuperState = true
      exitActions += "exit1"
      exitActions += "exit2"
    }
    syntax.machines.last.states += new State("entrySuper") {
      isSuperState = true
      entryActions += "entry1"
      entryActions += "entry2"
    }
    syntax.machines.last.states += new State("exitState") {
      superStates += "exitSuper"
      exitActions += "exit"
      events += new Event("event") {
        targetState = "entryState"
        actions += "action"
      }
    }
    syntax.machines.last.states += new State("entryState") {
      superStates += "entrySuper"
      entryActions += "entry"
      events += new Event("event") {
        targetState = "exitState"
      }
    }

    val machines = optimize()
    assertTransition(machines.head.transitions.head,
      "exitState", "event", "entryState", ListBuffer(
        "exit", "exit1", "exit2", "entry1", "entry2", "entry", "action"))
    assertTransition(machines.head.transitions.last,
      "entryState", "event", "exitState", ListBuffer.empty)
  }

  test("Multiple superstate entry and exit actions collected in the right order") {
    syntax.machines.last.states += new State("exitSuper1") {
      isSuperState = true
      exitActions += "exit1"
    }
    syntax.machines.last.states += new State("exitSuper2") {
      isSuperState = true
      superStates += "exitSuper1"
      exitActions += "exit2"
    }
    syntax.machines.last.states += new State("entrySuper1") {
      isSuperState = true
      entryActions += "entry1"
    }
    syntax.machines.last.states += new State("entrySuper2") {
      isSuperState = true
      superStates += "entrySuper1"
      entryActions += "entry2"
    }
    syntax.machines.last.states += new State("exitState") {
      superStates += "exitSuper2"
      exitActions += "exit"
      events += new Event("event") {
        targetState = "entryState"
        actions += "action"
      }
    }
    syntax.machines.last.states += new State("entryState") {
      superStates += "entrySuper2"
      entryActions += "entry"
      events += new Event("event") {
        targetState = "exitState"
      }
    }

    val machines = optimize()
    assertTransition(machines.head.transitions.head,
      "exitState", "event", "entryState", ListBuffer(
        "exit", "exit1", "exit2", "entry1", "entry2", "entry", "action"))
    assertTransition(machines.head.transitions.last,
      "entryState", "event", "exitState", ListBuffer.empty)
  }

  test("Diamond hierarchy entry and exit actions collected in the right order") {
    syntax.machines.last.states += new State("exitSuper1") {
      isSuperState = true
      exitActions += "exit1"
    }
    syntax.machines.last.states += new State("exitSuper2") {
      isSuperState = true
      superStates += "exitSuper1"
      exitActions += "exit2"
    }
    syntax.machines.last.states += new State("exitSuper3") {
      isSuperState = true
      superStates += "exitSuper1"
      exitActions += "exit3"
    }
    syntax.machines.last.states += new State("entrySuper1") {
      isSuperState = true
      entryActions += "entry1"
    }
    syntax.machines.last.states += new State("entrySuper2") {
      isSuperState = true
      superStates += "entrySuper1"
      entryActions += "entry2"
    }
    syntax.machines.last.states += new State("entrySuper3") {
      isSuperState = true
      superStates += "entrySuper1"
      entryActions += "entry3"
    }
    syntax.machines.last.states += new State("exitState") {
      superStates += "exitSuper2"
      superStates += "exitSuper3"
      exitActions += "exit"
      events += new Event("event") {
        targetState = "entryState"
        actions += "action"
      }
    }
    syntax.machines.last.states += new State("entryState") {
      superStates += "entrySuper2"
      superStates += "entrySuper3"
      entryActions += "entry"
      events += new Event("event") {
        targetState = "exitState"
      }
    }

    val machines = optimize()
    assertTransition(machines.head.transitions.head, "exitState", "event", "entryState",
      ListBuffer("exit", "exit3", "exit2", "exit1", "entry1", "entry2", "entry3", "entry", "action"))
    assertTransition(machines.head.transitions.last,
      "entryState", "event", "exitState", ListBuffer.empty)
  }
}
