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

  test("Parse transition with single action") {
    assertParsed("$state St => Ev => - => Ac", "    St Ev St Ac")
  }

  test("Parse transition with single grouped action, treating it as single action") {
    assertParsed("$state St => Ev => - => {Ac}", "    St Ev St Ac")
  }

  test("Parse transition with multiple grouped actions") {
    assertParsed("$state St => Ev => - => {Ac Ac}", "    St Ev St {Ac Ac}")
  }
}

class SubtransitionSyntaxSuite extends SyntacticalSuite {
  override protected def assertParsed(input: String, output: String): Unit = {
    val inputWithBoilerplate = s"$$machine MyMachine => Init {$input}"
    val outputWithBoilerplate = s"{\n  machine MyMachine initial Init\n$output\n}"
    super.assertParsed(inputWithBoilerplate, outputWithBoilerplate)
  }

  test("Parse empty subtransition") {
    assertParsed("$state St {}", "    St ")
  }

  test("Parse simple subtransition") {
    assertParsed("$state St { $event Ev => De }", "    St Ev De")
  }

  test("Parse subtransition with hyphen destination") {
    assertParsed("$state St { $event Ev => - }", "    St Ev St")
  }

  test("Parse subtransition with single action") {
    assertParsed("$state St { $event Ev => - => Ac }", "    St Ev St Ac")
  }

  test("Parse subtransition with single grouped action, treating it as single action") {
    assertParsed("$state St { $event Ev => - => {Ac} }", "    St Ev St Ac")
  }

  test("Parse subtransition with multiple grouped actions") {
    assertParsed("$state St { $event Ev => - => {Ac Ac} }", "    St Ev St {Ac Ac}")
  }

  test("Parse subtransition with multiple events") {
    assertParsed("$state St1 { $event Ev1 => - => {Ac1 Ac2} $event Ev2 => St2 => {Ac3} }",
      "    St1 {\n" +
      "      Ev1 St1 {Ac1 Ac2}\n" +
      "      Ev2 St2 Ac3\n" +
      "    }")
  }

  test("Parse subtransition with single entry action") {
    assertParsed("$state S { $event E => - $entry A }", "    S ENA E S")
  }

  test("Parse subtransition with multiple entry actions") {
    assertParsed("$state S { $event E => - $entry {A1 A2} }", "    S ENA1 ENA2 E S")
  }

  test("Parse subtransition with single exit action") {
    assertParsed("$state S { $event E => - $exit A }", "    S EXA E S")
  }

  test("Parse subtransition with multiple exit actions") {
    assertParsed("$state S { $event E => - $exit {A1 A2} }", "    S EXA1 EXA2 E S")
  }

  test("Parse subtransition with inheritance") {
    assertParsed("$state S $inherits SU { $event E => - }", "    S SUSU E S")
  }
}

class SuperstateSyntaxSuite extends SyntacticalSuite {
  override protected def assertParsed(input: String, output: String): Unit = {
    val inputWithBoilerplate = s"$$machine MyMachine => Init {$input}"
    val outputWithBoilerplate = s"{\n  machine MyMachine initial Init\n$output\n}"
    super.assertParsed(inputWithBoilerplate, outputWithBoilerplate)
  }

  test("Parse empty superstate") {
    assertParsed("$superstate Su {}", "    (Su) ")
  }

  test("Parse simple superstate") {
    assertParsed("$superstate Su { $event Ev => De }", "    (Su) Ev De")
  }

  test("Parse superstate with hyphen destination") {
    assertParsed("$superstate Su { $event Ev => - }", "    (Su) Ev Su")
  }

  test("Parse superstate with single action") {
    assertParsed("$superstate Su { $event Ev => - => Ac }", "    (Su) Ev Su Ac")
  }

  test("Parse superstate with single grouped action, treating it as single action") {
    assertParsed("$superstate Su { $event Ev => - => {Ac} }", "    (Su) Ev Su Ac")
  }

  test("Parse superstate with multiple grouped actions") {
    assertParsed("$superstate Su { $event Ev => - => {Ac Ac} }", "    (Su) Ev Su {Ac Ac}")
  }

  test("Parse superstate with multiple events") {
    assertParsed("$superstate Su { $event Ev1 => - => {Ac1 Ac2} $event Ev2 => St2 => {Ac3} }",
      "    (Su) {\n" +
        "      Ev1 Su {Ac1 Ac2}\n" +
        "      Ev2 St2 Ac3\n" +
        "    }")
  }

  test("Parse superstate with single entry action") {
    assertParsed("$superstate Su { $event E => - $entry A }", "    (Su) ENA E Su")
  }

  test("Parse superstate with multiple entry actions") {
    assertParsed("$superstate Su { $event E => - $entry {A1 A2} }", "    (Su) ENA1 ENA2 E Su")
  }

  test("Parse superstate with single exit action") {
    assertParsed("$superstate Su { $event E => - $exit A }", "    (Su) EXA E Su")
  }

  test("Parse superstate with multiple exit actions") {
    assertParsed("$superstate Su { $event E => - $exit {A1 A2} }", "    (Su) EXA1 EXA2 E Su")
  }

  test("Parse superstate which has inheritance of its own") {
    assertParsed("$superstate SU1 $inherits SU2 { $event E => - }", "    (SU1) SUSU2 E SU1")
  }
}
