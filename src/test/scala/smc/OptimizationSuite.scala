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
        isAbstract = true
      }
      states += new State("state3") {
        isAbstract = true
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
      isAbstract = true
      exitActions += "exit1"
      exitActions += "exit2"
    }
    syntax.machines.last.states += new State("entrySuper") {
      isAbstract = true
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
      isAbstract = true
      exitActions += "exit1"
    }
    syntax.machines.last.states += new State("exitSuper2") {
      isAbstract = true
      superStates += "exitSuper1"
      exitActions += "exit2"
    }
    syntax.machines.last.states += new State("entrySuper1") {
      isAbstract = true
      entryActions += "entry1"
    }
    syntax.machines.last.states += new State("entrySuper2") {
      isAbstract = true
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
      isAbstract = true
      exitActions += "exit1"
    }
    syntax.machines.last.states += new State("exitSuper2") {
      isAbstract = true
      superStates += "exitSuper1"
      exitActions += "exit2"
    }
    syntax.machines.last.states += new State("exitSuper3") {
      isAbstract = true
      superStates += "exitSuper1"
      exitActions += "exit3"
    }
    syntax.machines.last.states += new State("entrySuper1") {
      isAbstract = true
      entryActions += "entry1"
    }
    syntax.machines.last.states += new State("entrySuper2") {
      isAbstract = true
      superStates += "entrySuper1"
      entryActions += "entry2"
    }
    syntax.machines.last.states += new State("entrySuper3") {
      isAbstract = true
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

class SuperstateOptimizerSuite extends OptimizationSuite {
  override def beforeEach(context: BeforeEach): Unit =
    syntax = new StateMachineSyntax() {
      machines += new StateMachine("machine")
    }

  private def assertTransition(transition: OptimizedTransition, state: String, event: String,
                               nextState: String, actions: ListBuffer[String]): Unit = {
    assertEquals(transition.currentState, state)
    assertEquals(transition.subTransitions.head.event, event)
    assertEquals(transition.subTransitions.head.nextState, nextState)
    assertEquals(transition.subTransitions.head.actions, actions)
  }

  private def assertLastTransition(transition: OptimizedTransition, state: String, event: String,
                               nextState: String, actions: ListBuffer[String]): Unit = {
    assertEquals(transition.currentState, state)
    assertEquals(transition.subTransitions.last.event, event)
    assertEquals(transition.subTransitions.last.nextState, nextState)
    assertEquals(transition.subTransitions.last.actions, actions)
  }

  test("Simple inheritance of transitions") {
    syntax.machines.last.states += new State("super") {
      isAbstract = true
      events += new Event("superEvent") {
        targetState = "state2"
        actions += "superAction"
      }
    }
    syntax.machines.last.states += new State("state1") {
      superStates += "super"
      events += new Event("event") {
        targetState = "state2"
        actions += "action"
      }
    }
    syntax.machines.last.states += new State("state2") {
      events += new Event("event") {
        targetState = "state1"
      }
    }

    val machine = optimize().head
    assertTransition(machine.transitions.head, "state1", "event", "state2", ListBuffer("action"))
    assertLastTransition(machine.transitions.head, "state1", "superEvent", "state2", ListBuffer("superAction"))
    assertTransition(machine.transitions.last, "state2", "event", "state1", ListBuffer.empty)
  }

  test("Complicated inheritance of transitions") {
    syntax.machines.last.states += new State("super1") {
      isAbstract = true
      events += new Event("superEvent1") {
        targetState = "state1"
        actions += "superAction1"
      }
      events += new Event("superEvent2") {
        targetState = "state2"
        actions += "superAction2"
      }
    }
    syntax.machines.last.states += new State("super2") {
      isAbstract = true
      superStates += "super1"
      events += new Event("superEvent") {
        targetState = "state2"
        actions += "superAction"
      }
    }
    syntax.machines.last.states += new State("state1") {
      superStates += "super2"
      events += new Event("event") {
        targetState = "state2"
        actions += "action"
      }
    }
    syntax.machines.last.states += new State("state2") {
      events += new Event("event") {
        targetState = "state1"
      }
    }

    val machine = optimize().head

    assertEquals(machine.transitions.head.currentState, "state1")
    assertEquals(machine.transitions.head.subTransitions.map(_.event),
      ListBuffer("event", "superEvent", "superEvent1", "superEvent2"))
    assertEquals(machine.transitions.head.subTransitions.map(_.actions), ListBuffer(
      ListBuffer("action"), ListBuffer("superAction"), ListBuffer("superAction1"), ListBuffer("superAction2")))

    assertTransition(machine.transitions.last, "state2", "event", "state1", ListBuffer.empty)
  }

  test("Multiple inheritance of transitions") {
    syntax.machines.last.states += new State("super1") {
      isAbstract = true
      events += new Event("superEvent1") {
        targetState = "state2"
        actions += "superAction1"
      }
    }
    syntax.machines.last.states += new State("super2") {
      isAbstract = true
      events += new Event("superEvent2") {
        targetState = "state2"
        actions += "superAction2"
      }
    }
    syntax.machines.last.states += new State("state1") {
      superStates += "super1"
      superStates += "super2"
      events += new Event("event") {
        targetState = "state2"
        actions += "action"
      }
    }
    syntax.machines.last.states += new State("state2") {
      events += new Event("event") {
        targetState = "state1"
      }
    }

    val machine = optimize().head

    assertEquals(machine.transitions.head.currentState, "state1")
    assertEquals(machine.transitions.head.subTransitions.map(_.event),
      ListBuffer("event", "superEvent1", "superEvent2"))
    assertEquals(machine.transitions.head.subTransitions.map(_.actions), ListBuffer(
      ListBuffer("action"), ListBuffer("superAction1"), ListBuffer("superAction2")))

    assertTransition(machine.transitions.last, "state2", "event", "state1", ListBuffer.empty)
  }

  test("Diamond of death inheritance of transitions") {
    syntax.machines.last.states += new State("superRoot") {
      isAbstract = true
      events += new Event("rootEvent") {
        targetState = "state2"
        actions += "rootAction"
      }
    }
    syntax.machines.last.states += new State("super1") {
      isAbstract = true
      superStates += "superRoot"
      events += new Event("event1") {
        targetState = "state2"
        actions += "action1"
      }
    }
    syntax.machines.last.states += new State("super2") {
      isAbstract = true
      superStates += "superRoot"
      events += new Event("event2") {
        targetState = "state2"
        actions += "action2"
      }
    }
    syntax.machines.last.states += new State("state1") {
      superStates += "super1"
      superStates += "super2"
      events += new Event("event") {
        targetState = "state2"
        actions += "action"
      }
    }
    syntax.machines.last.states += new State("state2") {
      events += new Event("event") {
        targetState = "state1"
      }
    }

    val machine = optimize().head

    assertEquals(machine.transitions.head.currentState, "state1")
    assertEquals(machine.transitions.head.subTransitions.map(_.event),
      ListBuffer("event", "event1", "rootEvent", "event2"))
    assertEquals(machine.transitions.head.subTransitions.map(_.actions), ListBuffer(
      ListBuffer("action"), ListBuffer("action1"), ListBuffer("rootAction"), ListBuffer("action2")))

    assertTransition(machine.transitions.last, "state2", "event", "state1", ListBuffer.empty)
  }

  test("Overriding transitions") {
    syntax.machines.last.states += new State("super") {
      isAbstract = true
      events += new Event("event") {
        targetState = "state3"
        actions += "action1"
      }
    }
    syntax.machines.last.states += new State("state1") {
      superStates += "super"
      events += new Event("event") {
        targetState = "state2"
        actions += "action2"
      }
    }
    syntax.machines.last.states += new State("state2") {
      events += new Event("event") {
        targetState = "state1"
      }
    }
    syntax.machines.last.states += new State("state3") {
      events += new Event("event") {
        targetState = "state1"
      }
    }

    val machine = optimize().head
    assertTransition(machine.transitions.head, "state1", "event", "state2", ListBuffer("action2"))
    assertTransition(machine.transitions(1), "state2", "event", "state1", ListBuffer.empty)
    assertTransition(machine.transitions(2), "state3", "event", "state1", ListBuffer.empty)
  }

  test("Get rid of duplicate transitions") {
    syntax.machines.last.states += new State("super") {
      isAbstract = true
      events += new Event("event") {
        targetState = "state2"
        actions += "action"
      }
    }
    syntax.machines.last.states += new State("state1") {
      superStates += "super"
      events += new Event("event") {
        targetState = "state2"
        actions += "action"
      }
    }
    syntax.machines.last.states += new State("state2") {
      events += new Event("event") {
        targetState = "state1"
      }
    }

    val machine = optimize().head
    assertTransition(machine.transitions.head, "state1", "event", "state2", ListBuffer("action"))
    assertTransition(machine.transitions.last, "state2", "event", "state1", ListBuffer.empty)
  }
}

class HappyPathOptimizerSuite extends OptimizationSuite {
  private def assertTransition(transition: OptimizedTransition, state: String, event: String,
                               nextState: String, actions: ListBuffer[String]): Unit = {
    assertEquals(transition.currentState, state)
    assertEquals(transition.subTransitions.head.event, event)
    assertEquals(transition.subTransitions.head.nextState, nextState)
    assertEquals(transition.subTransitions.head.actions, actions)
  }

  test("Optimized sample coffee machine FSM") {
    syntax.machines += new StateMachine("CoffeeMachine") {
      initialState = "Selecting"
      states += new State("Selecting") {
        events += new Event("ChooseDrink") {
          targetState = "Brewing"
        }
      }
      states += new State("Brewing") {
        events += new Event("Finish") {
          targetState = "Selecting"
          actions += "dispenseCup"
        }
      }
    }

    val machine = optimize().head
    assertEquals(machine.name, "CoffeeMachine")
    assertEquals(machine.initialState, "Selecting")
    assertTransition(machine.transitions.head, "Selecting", "ChooseDrink", "Brewing", ListBuffer.empty)
    assertTransition(machine.transitions.last, "Brewing", "Finish", "Selecting", ListBuffer("dispenseCup"))
  }

  test("Optimized more complicated sample coffee machine FSM") {
    syntax.machines += new StateMachine("CoffeeMachine") {
      initialState = "Selecting"
      states += new State("Operational") {
        isAbstract = true
        events += new Event("PowerOutage") {
          targetState = "Off"
        }
      }
      states += new State("Selecting") {
        superStates += "Operational"
        entryActions += "DisplayMenu"
        events += new Event("ChooseDrink") {
          targetState = "Brewing"
        }
        events += new Event("NoMoreCoffee") {
          targetState = "Selecting"
        }
      }
      states += new State("Brewing") {
        superStates += "Operational"
        entryActions += "StartHeating"
        exitActions += "StopHeating"
        events += new Event("Finish") {
          targetState = "Selecting"
          actions += "dispenseCup"
        }
      }
      states += new State("Off") {
        entryActions += "PowerDownComponents"
        events += new Event("PowerRestored") {
          targetState = "Selecting"
        }
      }
    }

    val machine = optimize().head
    assertEquals(machine.name, "CoffeeMachine")
    assertEquals(machine.initialState, "Selecting")

    assertEquals(machine.transitions.head.currentState, "Selecting")
    assertEquals(machine.transitions.head.subTransitions.map(_.event),
      ListBuffer("ChooseDrink", "NoMoreCoffee", "PowerOutage"))
    assertEquals(machine.transitions.head.subTransitions.map(_.nextState),
      ListBuffer("Brewing", "Selecting", "Off"))
    assertEquals(machine.transitions.head.subTransitions.map(_.actions), ListBuffer(
      ListBuffer("StartHeating"), ListBuffer.empty, ListBuffer("PowerDownComponents")))

    assertEquals(machine.transitions(1).currentState, "Brewing")
    assertEquals(machine.transitions(1).subTransitions.map(_.event),
      ListBuffer("Finish", "PowerOutage"))
    assertEquals(machine.transitions(1).subTransitions.map(_.nextState),
      ListBuffer("Selecting", "Off"))
    assertEquals(machine.transitions(1).subTransitions.map(_.actions), ListBuffer(
      ListBuffer("StopHeating", "DisplayMenu", "dispenseCup"), ListBuffer("StopHeating", "PowerDownComponents")))

    assertTransition(machine.transitions.last, "Off", "PowerRestored", "Selecting", ListBuffer("DisplayMenu"))
  }

  test("Optimized printer sample FSM") {
    syntax.machines += new StateMachine("Printer") {
      initialState = "Idle"
      states += new State("Operational") {
        isAbstract = true
        events += new Event("PowerLoss") {
          targetState = "Offline"
        }
      }
      states += new State("Idle") {
        superStates += "Operational"
        events += new Event("Print") {
          targetState = "Printing"
        }
        events += new Event("Cancel") {
          targetState = "Idle"
          actions += "logCancelWithoutJob"
          actions += "beep"
        }
      }
      states += new State("Printing") {
        superStates += "Operational"
        events += new Event("Finish") {
          targetState = "Idle"
          actions += "ejectPage"
        }
      }
      states += new State("Offline") {
        entryActions += "PowerDown"
        events += new Event("PowerRestored") {
          targetState = "Idle"
        }
      }
    }

    val machine = optimize().head
    assertEquals(machine.name, "Printer")
    assertEquals(machine.initialState, "Idle")

    assertEquals(machine.transitions.head.currentState, "Idle")
    assertEquals(machine.transitions.head.subTransitions.map(_.event),
      ListBuffer("Print", "Cancel", "PowerLoss"))
    assertEquals(machine.transitions.head.subTransitions.map(_.nextState),
      ListBuffer("Printing", "Idle", "Offline"))
    assertEquals(machine.transitions.head.subTransitions.map(_.actions), ListBuffer(
      ListBuffer.empty, ListBuffer("logCancelWithoutJob", "beep"), ListBuffer("PowerDown")))

    assertEquals(machine.transitions(1).currentState, "Printing")
    assertEquals(machine.transitions(1).subTransitions.map(_.event),
      ListBuffer("Finish", "PowerLoss"))
    assertEquals(machine.transitions(1).subTransitions.map(_.nextState),
      ListBuffer("Idle", "Offline"))
    assertEquals(machine.transitions(1).subTransitions.map(_.actions), ListBuffer(
      ListBuffer("ejectPage"), ListBuffer("PowerDown")))

    assertTransition(machine.transitions.last, "Offline", "PowerRestored", "Idle", ListBuffer.empty)
  }
}
