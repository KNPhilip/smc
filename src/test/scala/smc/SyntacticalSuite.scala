package smc

import munit.FunSuite
import smc.lexicalAnalyzer.LexicalAnalyzer
import smc.syntaxAnalyzer.SyntacticalAnalyzer

class SyntacticalSuite extends FunSuite {
  protected val parser: SyntacticalAnalyzer = new SyntacticalAnalyzer()
  private val lexer: LexicalAnalyzer = new LexicalAnalyzer(parser)

  override def beforeEach(context: BeforeEach): Unit = {
    parser.clear()
  }

  protected def assertParsed(input: String, output: String): Unit = {
    lexer.lex(input)
    assertEquals(parser.getStateMachineSyntax.toString.trim, output)
  }
}

class MachineSyntaxSuite extends SyntacticalSuite {
  test("Parse empty state machine with no initial state") {
    assertParsed("$machine MyMachine {}", "{\n  machine MyMachine \n}")
  }

  test("Parse empty state machine with initial state") {
    assertParsed("$machine MyMachine { $initial Init }", "{\n  machine MyMachine initial Init\n}")
  }

  test("Parse empty state machine with initial state using arrow syntax") {
    assertParsed("$machine MyMachine => Init {}", "{\n  machine MyMachine initial Init\n}")
  }

  test("Parse empty state machine with multiple initial states") {
    assertParsed("$machine MyMachine => Init { $initial Init $initial NewestInit }",
      "{\n  machine MyMachine initial NewestInit\n}")
  }
}

class TransitionSyntaxSuite extends SyntacticalSuite {
  override protected def assertParsed(input: String, output: String): Unit = {
    val inputWithBoilerplate = s"$$machine MyMachine => Init {$input}"
    val outputWithBoilerplate = s"{\n  machine MyMachine initial Init\n$output\n}"
    super.assertParsed(inputWithBoilerplate, outputWithBoilerplate)
  }

  test("Parse simple transition") {
    assertParsed("$state St => Ev => De", "    St Ev De")
  }

  test("Parse transition with hyphen destination") {
    assertParsed("$state St => Ev => - ", "    St Ev St")
  }

  test("Parse transition with actions") {
    assertParsed("$state St => Ev => - => Ac", "    St Ev St Ac")
  }

  test("Parse transition with single grouped actions, treating it as single action") {
    assertParsed("$state St => Ev => - => {Ac}", "    St Ev St Ac")
  }

  test("Parse transition with multiple grouped actions") {
    assertParsed("$state St => Ev => - => {Ac Ac}", "    St Ev St {Ac Ac}")
  }
}
