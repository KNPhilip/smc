package smc

import munit.FunSuite
import scala.collection.mutable.ListBuffer
import smc.lexicalAnalyzer.LexicalAnalyzer
import smc.syntaxAnalyzer.{Event, State, StateMachine, StateMachineSyntax, SyntacticalAnalyzer, SyntaxError}

class SyntacticalSuite extends FunSuite {
  protected val parser: SyntacticalAnalyzer = new SyntacticalAnalyzer()
  private val lexer: LexicalAnalyzer = new LexicalAnalyzer(parser)

  override def beforeEach(context: BeforeEach): Unit = {
    parser.clear()
  }

  protected def assertParsed(input: String, output: String): Unit = {
    lexer.lex(input)
    val parsed = StateMachineRenderer.render(parser.getStateMachineSyntax)
    assertEquals(parsed.trim, output.trim)
  }

  protected def assertParseError(input: String, output: String): Unit = {
    lexer.lex(input)
    val parsedErrors = StateMachineRenderer.render(parser.getStateMachineSyntax.errors)
    assertEquals(parsedErrors.trim, output.trim)
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
    assertParsed("$state S { $event E => - $entry A1 $entry A2 }", "    S ENA1 ENA2 E S")
  }

  test("Parse subtransition with multiple entry actions grouped by braces") {
    assertParsed("$state S { $event E => - $entry {A1 A2} }", "    S ENA1 ENA2 E S")
  }

  test("Parse subtransition with single exit action") {
    assertParsed("$state S { $event E => - $exit A }", "    S EXA E S")
  }

  test("Parse subtransition with multiple exit actions") {
    assertParsed("$state S { $event E => - $exit A1 $exit A2 }", "    S EXA1 EXA2 E S")
  }

  test("Parse subtransition with multiple exit actions grouped by braces") {
    assertParsed("$state S { $event E => - $exit {A1 A2} }", "    S EXA1 EXA2 E S")
  }

  test("Parse subtransition with multiple entry and exit actions") {
    assertParsed("$state S { $event E => - $entry {A1 A2} $exit {A3 A4 A5} }",
      "    S ENA1 ENA2 EXA3 EXA4 EXA5 E S")
  }

  test("Parse subtransition with inheritance") {
    assertParsed("$state S : SU { $event E => - }", "    S SUSU E S")
  }

  test("Parse subtransition with multiple inheritance") {
    assertParsed("$state S : SU1 : SU2 { $event E => - }", "    S SUSU1 SUSU2 E S")
  }
}

class AbstractStateSyntaxSuite extends SyntacticalSuite {
  override protected def assertParsed(input: String, output: String): Unit = {
    val inputWithBoilerplate = s"$$machine MyMachine => Init {$input}"
    val outputWithBoilerplate = s"{\n  machine MyMachine initial Init\n$output\n}"
    super.assertParsed(inputWithBoilerplate, outputWithBoilerplate)
  }

  test("Parse empty abstract state") {
    assertParsed("$abstract Su {}", "    (Su) ")
  }

  test("Parse simple abstract state") {
    assertParsed("$abstract Su { $event Ev => De }", "    (Su) Ev De")
  }

  test("Parse abstract state with hyphen destination") {
    assertParsed("$abstract Su { $event Ev => - }", "    (Su) Ev Su")
  }

  test("Parse abstract state with single action") {
    assertParsed("$abstract Su { $event Ev => - => Ac }", "    (Su) Ev Su Ac")
  }

  test("Parse abstract state with single grouped action, treating it as single action") {
    assertParsed("$abstract Su { $event Ev => - => {Ac} }", "    (Su) Ev Su Ac")
  }

  test("Parse abstract state with multiple grouped actions") {
    assertParsed("$abstract Su { $event Ev => - => {Ac Ac} }", "    (Su) Ev Su {Ac Ac}")
  }

  test("Parse abstract state with multiple events") {
    assertParsed("$abstract Su { $event Ev1 => - => {Ac1 Ac2} $event Ev2 => St2 => {Ac3} }",
      "    (Su) {\n" +
      "      Ev1 Su {Ac1 Ac2}\n" +
      "      Ev2 St2 Ac3\n" +
      "    }")
  }

  test("Parse abstract state with single entry action") {
    assertParsed("$abstract Su { $event E => - $entry A }", "    (Su) ENA E Su")
  }

  test("Parse abstract state with multiple entry actions") {
    assertParsed("$abstract Su { $event E => - $entry {A1 A2} }", "    (Su) ENA1 ENA2 E Su")
  }

  test("Parse abstract state with single exit action") {
    assertParsed("$abstract Su { $event E => - $exit A }", "    (Su) EXA E Su")
  }

  test("Parse abstract state with multiple exit actions") {
    assertParsed("$abstract Su { $event E => - $exit {A1 A2} }", "    (Su) EXA1 EXA2 E Su")
  }

  test("Parse abstract state which has inheritance of its own") {
    assertParsed("$abstract SU1 : SU2 { $event E => - }", "    (SU1) SUSU2 E SU1")
  }
}

class ComplicatedSyntaxSuite extends SyntacticalSuite {
  test("Parse sample coffee machine FSM") {
    assertParsed(
      "$machine \"CoffeeMachine\" {\n" +
      "  $initial \"Selecting\"\n" +
      "  $state \"Selecting\" => \"ChooseDrink\" => \"Brewing\"\n" +
      "  $state \"Brewing\" => \"Finish\" => \"Selecting\" => \"dispenseCup\"\n" +
      "}",
      "{\n" +
      "  machine CoffeeMachine initial Selecting\n" +
      "    Selecting ChooseDrink Brewing\n" +
      "    Brewing Finish Selecting dispenseCup\n" +
      "}")
  }

  test("Parse more complicated sample coffee machine FSM") {
    assertParsed(
      "$machine CoffeeMachine {\n" +
      "  $initial Selecting\n" +
      "  $abstract Operational {\n" +
      "    $event PowerOutage => Off\n" +
      "  }\n" +
      "  $state Selecting : Operational {\n" +
      "    $entry DisplayMenu\n" +
      "    $event ChooseDrink => Brewing\n" +
      "    $event NoMoreCoffee => Selecting\n" +
      "  }\n" +
      "  $state Brewing : Operational {\n" +
      "    $entry StartHeating\n" +
      "    $exit StopHeating\n" +
      "    $event Finish => Selecting => dispenseCup\n" +
      "  }\n" +
      "  $state Off {\n" +
      "    $entry PowerDownComponents\n" +
      "    $event PowerRestored => Selecting\n" +
      "  }\n" +
      "}",
      "{\n" +
      "  machine CoffeeMachine initial Selecting\n" +
      "    (Operational) PowerOutage Off\n" +
      "    Selecting SUOperational ENDisplayMenu {\n" +
      "      ChooseDrink Brewing\n" +
      "      NoMoreCoffee Selecting\n" +
      "    }\n" +
      "    Brewing SUOperational ENStartHeating EXStopHeating Finish Selecting dispenseCup\n" +
      "    Off ENPowerDownComponents PowerRestored Selecting\n" +
      "}")
  }

  test("Parse syntax sugar sample printer FSM") {
    assertParsed(
      "$machine Printer => Idle {\n" +
      "  $abstract Operational {\n" +
      "    $event PowerLoss => Offline\n" +
      "  }\n" +
      "  $state Idle : Operational {\n" +
      "    $event Print => Printing\n" +
      "    $event Cancel => - => {logCancelWithoutJob beep}\n" +
      "  }\n" +
      "  $state Printing : Operational {\n" +
      "    $event Finish => Idle => ejectPage\n" +
      "  }\n" +
      "  $state Offline {\n" +
      "    $entry PowerDown\n" +
      "    $event PowerRestored => Idle\n" +
      "  }\n" +
      "}",
      "{\n" +
        "  machine Printer initial Idle\n" +
        "    (Operational) PowerLoss Offline\n" +
        "    Idle SUOperational {\n" +
        "      Print Printing\n" +
        "      Cancel Idle {logCancelWithoutJob beep}\n" +
        "    }\n" +
        "    Printing SUOperational Finish Idle ejectPage\n" +
        "    Offline ENPowerDown PowerRestored Idle\n" +
        "}")
  }
}

class ErrorSyntaxSuite extends SyntacticalSuite {
  test("Parser gives unknown syntax error") {
    assertParseError("¤#.%!€&?",
      "Syntax error: SyntaxError. . line 1, position 1.")
  }

  test("Line number and positions are functioning") {
    assertParseError("$machine m {\n$initial %#!€\n}",
      "Syntax error: SyntaxError. . line 2, position 10.")
  }

  test("Parser gives invalid machine error") {
    assertParseError("blah",
      "Syntax error: MachineError. MachineDeclaration|Name. line 1, position 0.")
  }

  test("Parser gives invalid transition error") {
    assertParseError("$machine m => i { $state }",
      "Syntax error: TransitionError. StateValue|ClosedBrace. line 1, position 25.")
  }

  test("Parser gives invalid subtransition error") {
    assertParseError("$machine m => i { $state s { $event } }",
      "Syntax error: SubtransitionError. SubeventValue|ClosedBrace. line 1, position 36.")
  }

  test("Parser gives invalid superstate error") {
    assertParseError("$machine m => i { $abstract }",
      "Syntax error: SuperstateError. SuperstateValue|ClosedBrace. line 1, position 28.")
  }

  test("Parser gives invalid entry action error") {
    assertParseError("$machine m => i { $state s { $entry } }",
      "Syntax error: EntryExitError. EntryDeclaration|ClosedBrace. line 1, position 36.")
  }

  test("Parser gives invalid exit action error") {
    assertParseError("$machine m => i { $state s { $exit } }",
      "Syntax error: EntryExitError. ExitDeclaration|ClosedBrace. line 1, position 35.")
  }
}

private object StateMachineRenderer {
  def render(errors: ListBuffer[SyntaxError]): String = {
    errors.collectFirst {
      case e if e != null =>
        s"Syntax error: ${e.errorType}. ${e.message}. " +
        s"line ${e.lineNumber}, position ${e.position}.\n"
    }.getOrElse("")
  }

  def render(syntax: StateMachineSyntax): String = {
    val rendered = syntax.machines
      .collect { case m if m != null => renderMachine(m) }

    if (rendered.nonEmpty)
      rendered.mkString("{\n", "", "}\n")
    else
      ""
  }

  private def renderMachine(machine: StateMachine): String = {
    val initial = Option(machine.initialState)
      .map(s => s"initial $s").getOrElse("")

    val states =
      machine.states
        .collect { case s if s != null => renderState(s) }
        .mkString("")

    s"  machine ${machine.name} $initial\n" + states
  }

  private def renderState(state: State): String = {
    val base =
      if (state.isAbstract) s"(${state.name})" else state.name

    val supers = state.superStates
      .collect { case s if s != null => " SU" + s }.mkString

    val entries = state.entryActions
      .collect { case a if a != null => " EN" + a }.mkString

    val exits = state.exitActions
      .collect { case a if a != null => " EX" + a }.mkString

    val events = renderEvents(state.events)

    s"    ${base + supers + entries + exits} $events\n"
  }

  private def renderEvents(input: ListBuffer[Event]): String = {
    val events = input.collect { case e if e != null => e }

    if (events.size == 1)
      renderEvent(events.head)
    else if (events.nonEmpty)
      events.map(e => "      " + renderEvent(e))
        .mkString("{\n", "\n", "\n    }")
    else
      ""
  }

  private def renderEvent(event: Event): String =
    List(
      event.name,
      Option(event.targetState).getOrElse(""),
      renderActions(event.actions)
    ).filter(_.nonEmpty).mkString(" ")

  private def renderActions(actions: ListBuffer[String]): String = {
    val acts = actions.collect { case a if a != null => a }

    if (acts.size == 1)
      acts.head
    else if (acts.nonEmpty)
      acts.mkString("{", " ", "}")
    else
      ""
  }
}
