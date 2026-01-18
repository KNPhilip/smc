package smc

import munit.FunSuite
import scala.collection.mutable.ListBuffer
import smc.semanticAnalyzer.SemanticError.*
import smc.semanticAnalyzer.{SemanticAnalyzer, SemanticError}
import smc.syntaxAnalyzer.{Event, State, StateMachine, StateMachineSyntax}

class SemanticSuite extends FunSuite {
  protected var syntax = new StateMachineSyntax()
  private val analyzer = new SemanticAnalyzer()
  private var semanticErrors: ListBuffer[SemanticError] = ListBuffer.empty

  override def beforeEach(context: BeforeEach): Unit =
    syntax = new StateMachineSyntax()

  protected def analyzeSyntax(): Unit =
    semanticErrors = analyzer.analyze(syntax)

  protected def assertPresentErrors(errors: SemanticError*): Unit =
    assert(errors.toSet.subsetOf(semanticErrors.toSet))

  protected def assertNonPresentErrors(errors: SemanticError*): Unit =
    assert(errors.toSet.intersect(semanticErrors.toSet).isEmpty)
}

class SemanticMachineSuite extends SemanticSuite {
  test("No machines") {
    analyzeSyntax()
    assertPresentErrors(NO_MACHINES)
  }

  test("Two machines that are duplicates") {
    syntax.machines += new StateMachine("Fsm")
    syntax.machines += new StateMachine("Fsm")
    analyzeSyntax()
    assertPresentErrors(DUPLICATE_MACHINE)
    assertNonPresentErrors(NO_MACHINES)
  }

  test("Machine without initial state") {
    syntax.machines += new StateMachine("Fsm")
    analyzeSyntax()
    assertPresentErrors(NO_INITIAL_STATE)
    assertNonPresentErrors(NO_MACHINES, DUPLICATE_MACHINE)
  }

  test("Machine without any transitions") {
    syntax.machines += new StateMachine("Fsm") {
      initialState = "initial"
    }

    analyzeSyntax()
    assertPresentErrors(NO_TRANSITIONS)
    assertNonPresentErrors(NO_MACHINES, DUPLICATE_MACHINE, NO_INITIAL_STATE)
  }

  test("Machine with initial state that is undefined") {
    syntax.machines += new StateMachine("Fsm") {
      initialState = "initial"
    }

    analyzeSyntax()
    assertPresentErrors(UNDEFINED_INITIAL_STATE)
    assertNonPresentErrors(NO_MACHINES, DUPLICATE_MACHINE, NO_INITIAL_STATE)
  }

  test("Machine with valid initial state") {
    syntax.machines += new StateMachine("Fsm") {
      initialState = "initial"
      states += new State("initial")
    }

    analyzeSyntax()
    assertNonPresentErrors(NO_MACHINES, DUPLICATE_MACHINE, NO_INITIAL_STATE, UNDEFINED_INITIAL_STATE, NO_TRANSITIONS)
  }
}

class SemanticStateSuite extends SemanticSuite {
  override def beforeEach(context: BeforeEach): Unit = {
    super.beforeEach(context)

    syntax.machines += new StateMachine("Fsm") {
      initialState = "initial"
      states += new State("initial")
    }
  }

  test("Next state is undefined") {
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "undefined"
      }
    }

    analyzeSyntax()
    assertPresentErrors(UNDEFINED_NEXT_STATE)
  }

  test("Next state is defined") {
    syntax.machines.last.states += new State("destination")
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "destination"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(UNDEFINED_NEXT_STATE)
  }

  test("Next state being same state is defined") {
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "state"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(UNDEFINED_NEXT_STATE)
  }

  test("Unused state") {
    syntax.machines.last.states += new State("state")
    analyzeSyntax()
    assertPresentErrors(UNUSED_STATE)
    assertNonPresentErrors(UNDEFINED_NEXT_STATE)
  }

  test("No unused states") {
    syntax.machines.last.states.last.events += new Event("event") {
      targetState = "state"
    }
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(UNUSED_STATE, UNDEFINED_NEXT_STATE)
  }

  test("Duplicate state") {
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "initial"
      }
    }
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_STATE)
    assertNonPresentErrors(UNDEFINED_NEXT_STATE)
  }

  test("Non-Duplicate transitions") {
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(DUPLICATE_STATE, UNDEFINED_NEXT_STATE)
  }
}

class SemanticTransitionSuite extends SemanticSuite {
  override def beforeEach(context: BeforeEach): Unit = {
    super.beforeEach(context)

    syntax.machines += new StateMachine("Fsm") {
      initialState = "initial"
      states += new State("initial")
    }
  }

  test("Same transitions are duplicate transition") {
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "initial"
      }
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_TRANSITION)
  }

  test("Different target states under same events are still duplicates") {
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "initial"
      }
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_TRANSITION)
  }

  test("No duplicate transitions") {
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(DUPLICATE_TRANSITION)
  }
}

class SemanticSuperstateSuite extends SemanticSuite {
  override def beforeEach(context: BeforeEach): Unit = {
    super.beforeEach(context)

    syntax.machines += new StateMachine("Fsm") {
      initialState = "initial"
      states += new State("initial")
    }
  }

  test("Superstate is undefined") {
    syntax.machines.last.states += new State("state") {
      superStates += "superstate"
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertPresentErrors(UNDEFINED_SUPER_STATE)
  }

  test("Superstate is defined") {
    syntax.machines.last.states += new State("superstate") {
      isAbstract = true
    }
    syntax.machines.last.states += new State("state") {
      superStates += "superstate"
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(UNDEFINED_SUPER_STATE)
  }

  test("Superstates cannot be targets") {
    syntax.machines.last.states += new State("superstate") {
      isAbstract = true
    }
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "superstate"
      }
    }

    analyzeSyntax()
    assertPresentErrors(SUPERSTATE_AS_NEXT_STATE)
  }

  test("Superstates can be used as superstates") {
    syntax.machines.last.states += new State("superstate") {
      isAbstract = true
    }
    syntax.machines.last.states += new State("state") {
      superStates += "superstate"
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(UNDEFINED_SUPER_STATE, SUPERSTATE_AS_NEXT_STATE, SUPERSTATE_CONFLICT)
  }

  test("Conflicting destination between events in a state and a superstate") {
    syntax.machines.last.states += new State("superstate") {
      isAbstract = true
      events += new Event("event") {
        targetState = "initial"
      }
    }
    syntax.machines.last.states += new State("state") {
      superStates += "superstate"
      events += new Event("event") {
        targetState = "target"
      }
    }

    analyzeSyntax()
    assertPresentErrors(SUPERSTATE_CONFLICT)
  }

  test("Still finds conflicting superstates when multiple superstates are in scope") {
    syntax.machines.last.states += new State("superstate1") {
      isAbstract = true
      events += new Event("event1") {
        targetState = "initial"
      }
    }
    syntax.machines.last.states += new State("superstate2") {
      isAbstract = true
      events += new Event("event2") {
        targetState = "initial"
      }
    }
    syntax.machines.last.states += new State("state") {
      superStates += "superstate1"
      superStates += "superstate2"
      events += new Event("event2") {
        targetState = "target"
      }
    }

    analyzeSyntax()
    assertPresentErrors(SUPERSTATE_CONFLICT)
  }

  test("Identical transition in superstate is not a conflict") {
    syntax.machines.last.states += new State("superstate") {
      isAbstract = true
      events += new Event("event") {
        targetState = "initial"
      }
    }
    syntax.machines.last.states += new State("state") {
      superStates += "superstate"
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(SUPERSTATE_CONFLICT)
  }

  test("Inherited duplicate transition is detected") {
    syntax.machines.last.states += new State("superstate") {
      isAbstract = true
      events += new Event("reset") {
        targetState = "foo"
      }
    }
    syntax.machines.last.states += new State("initial") {
      superStates += "superstate"
      events += new Event("reset") {
        targetState = "bar"
      }
    }
    syntax.machines.last.states += new State("foo")
    syntax.machines.last.states += new State("bar")

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_TRANSITION)
  }
}

class SemanticDuplicateNameSuite extends SemanticSuite {
  override def beforeEach(context: BeforeEach): Unit = {
    super.beforeEach(context)

    syntax.machines += new StateMachine("Fsm") {
      initialState = "initial"
      states += new State("initial")
    }
  }

  test("Event name conflicts with transition action name") {
    syntax.machines.last.states += new State("state") {
      events += new Event("doSomething") {
        targetState = "initial"
      }
      events += new Event("trigger") {
        targetState = "initial"
        actions += "doSomething"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_NAME)
  }

  test("Event name conflicts with entry action name") {
    syntax.machines.last.states += new State("state") {
      entryActions += "initialize"
      events += new Event("initialize") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_NAME)
  }

  test("Event name conflicts with exit action name") {
    syntax.machines.last.states += new State("state") {
      exitActions += "cleanup"
      events += new Event("cleanup") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_NAME)
  }

  test("Duplicate action names in different transitions") {
    syntax.machines.last.states += new State("state1") {
      events += new Event("event1") {
        targetState = "initial"
        actions += "doAction"
      }
    }
    syntax.machines.last.states += new State("state2") {
      events += new Event("event2") {
        targetState = "initial"
        actions += "doAction"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_NAME)
  }

  test("Duplicate entry action names") {
    syntax.machines.last.states += new State("state1") {
      entryActions += "setup"
      events += new Event("event1") {
        targetState = "initial"
      }
    }
    syntax.machines.last.states += new State("state2") {
      entryActions += "setup"
      events += new Event("event2") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_NAME)
  }

  test("Duplicate exit action names") {
    syntax.machines.last.states += new State("state1") {
      exitActions += "teardown"
      events += new Event("event1") {
        targetState = "initial"
      }
    }
    syntax.machines.last.states += new State("state2") {
      exitActions += "teardown"
      events += new Event("event2") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_NAME)
  }

  test("Entry action conflicts with exit action") {
    syntax.machines.last.states += new State("state") {
      entryActions += "doSomething"
      exitActions += "doSomething"
      events += new Event("event") {
        targetState = "initial"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_NAME)
  }

  test("Multiple actions in same transition with same name") {
    syntax.machines.last.states += new State("state") {
      events += new Event("event") {
        targetState = "initial"
        actions += "action1"
        actions += "action2"
        actions += "action1"
      }
    }

    analyzeSyntax()
    assertPresentErrors(DUPLICATE_NAME)
  }

  test("No duplicate names when event and action are different") {
    syntax.machines.last.states += new State("state") {
      events += new Event("trigger") {
        targetState = "initial"
        actions += "doAction"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(DUPLICATE_NAME)
  }

  test("No duplicate names when multiple different actions exist") {
    syntax.machines.last.states += new State("state1") {
      entryActions += "setup1"
      exitActions += "teardown1"
      events += new Event("event1") {
        targetState = "initial"
        actions += "action1"
      }
    }
    syntax.machines.last.states += new State("state2") {
      entryActions += "setup2"
      exitActions += "teardown2"
      events += new Event("event2") {
        targetState = "initial"
        actions += "action2"
      }
    }

    analyzeSyntax()
    assertNonPresentErrors(DUPLICATE_NAME)
  }
}

class SemanticHappyPathSuite extends SemanticSuite {
  private def assertNoErrors(): Unit = {
    assertNonPresentErrors(
      NO_MACHINES,
      DUPLICATE_MACHINE,
      NO_INITIAL_STATE,
      NO_TRANSITIONS,
      UNDEFINED_INITIAL_STATE,
      UNDEFINED_NEXT_STATE,
      UNUSED_STATE,
      DUPLICATE_STATE,
      DUPLICATE_TRANSITION,
      UNDEFINED_SUPER_STATE,
      SUPERSTATE_AS_NEXT_STATE,
      SUPERSTATE_CONFLICT,
      DUPLICATE_NAME)
  }

  test("Sample coffee machine FSM is semantically correct") {
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
    analyzeSyntax()
    assertNoErrors()
  }

  test("More complicated sample coffee machine FSM is semantically correct") {
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
    analyzeSyntax()
    assertNoErrors()
  }

  test("Printer sample FSM is semantically correct") {
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
    analyzeSyntax()
    assertNoErrors()
  }
}
