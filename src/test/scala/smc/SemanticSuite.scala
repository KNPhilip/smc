package smc

import munit.FunSuite
import smc.semanticAnalyzer.SemanticError.*
import smc.semanticAnalyzer.{SemanticAnalyzer, SemanticError, SemanticSyntax}
import smc.syntaxAnalyzer.{Event, State, StateMachine, StateMachineSyntax}

class SemanticSuite extends FunSuite {
  protected var syntax = new StateMachineSyntax()
  private val analyzer = new SemanticAnalyzer()
  private var semanticSyntax = new SemanticSyntax()

  override def beforeEach(context: BeforeEach): Unit =
    syntax = new StateMachineSyntax()

  protected def analyzeSyntax(): Unit =
    semanticSyntax = analyzer.analyze(syntax)

  protected def assertPresentErrors(errors: SemanticError*): Unit = {
    val presentErrors = semanticSyntax.errors.toSet
    assert(errors.toSet.subsetOf(presentErrors))
  }

  protected def assertNonPresentErrors(errors: SemanticError*): Unit = {
    val commonErrors = errors.toSet.intersect(semanticSyntax.errors.toSet)
    assert(commonErrors.isEmpty)
  }
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
