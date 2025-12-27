package smc

import munit.FunSuite
import smc.semanticAnalyzer.SemanticError.*
import smc.semanticAnalyzer.{SemanticAnalyzer, SemanticError, SemanticSyntax}
import smc.syntaxAnalyzer.{StateMachine, StateMachineSyntax}

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
    val machine = new StateMachine("Fsm")
    machine.initialState = "initial"
    syntax.machines += machine

    analyzeSyntax()

    assertPresentErrors(NO_TRANSITIONS)
    assertNonPresentErrors(NO_MACHINES, DUPLICATE_MACHINE, NO_INITIAL_STATE)
  }

  test("Machine with initial state that is undefined") {
    val machine = new StateMachine("Fsm")
    machine.initialState = "initial"
    syntax.machines += machine

    analyzeSyntax()

    assertPresentErrors(UNDEFINED_INITIAL_STATE)
    assertNonPresentErrors(NO_MACHINES, DUPLICATE_MACHINE, NO_INITIAL_STATE)
  }
}
