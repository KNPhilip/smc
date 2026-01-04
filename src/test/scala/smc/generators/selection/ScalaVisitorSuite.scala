package smc.generators.selection

import munit.FunSuite
import smc.optimizer.OptimizedStateMachine
import smc.generators.OptimizedBuilder._
import smc.generators.selection.visitors.ScalaVisitor

final class ScalaVisitorSuite extends FunSuite {
  private val visitor = new ScalaVisitor()

  private def assertGenerated(machine: OptimizedStateMachine, expected: String): Unit = {
    SelectionNodeFactory.generate(machine).accept(visitor)
    assertEquals(visitor.getOutput, expected)
  }

  test("Scala source code generation of Coffee Machine FSM") {
    assertGenerated(
      sm("CoffeeMachine", "Selecting",
        states = Seq("Selecting", "Brewing"),
        events = Seq("ChooseDrink", "Finish"),
        actions = Seq("dispenseCup"),
        transitions = Seq(
          transition("Selecting", sub("ChooseDrink", "Brewing")),
          transition("Brewing", sub("Finish", "Selecting", "dispenseCup")))),
      "abstract class CoffeeMachine {\n" +
      "  def unhandledTransition(state: String, event: String): Unit\n" +
      "\n" +
      "  protected def dispenseCup(): Unit\n" +
      "\n" +
      "  def ChooseDrink(): Unit =\n" +
      "    handleEvent(Event.ChooseDrink)\n" +
      "\n" +
      "  def Finish(): Unit =\n" +
      "    handleEvent(Event.Finish)\n" +
      "\n" +
      "  private var state: State = State.Selecting\n" +
      "\n" +
      "  private def setState(s: State): Unit =\n" +
      "    state = s\n" +
      "\n" +
      "  private def handleEvent(event: Event): Unit = {\n" +
      "    state match {\n" +
      "      case State.Selecting =>\n" +
      "        event match {\n" +
      "          case Event.ChooseDrink =>\n" +
      "            setState(State.Brewing)\n" +
      "          case _ => unhandledTransition(state.toString, event.toString)\n" +
      "        }\n" +
      "      case State.Brewing =>\n" +
      "        event match {\n" +
      "          case Event.Finish =>\n" +
      "            setState(State.Selecting)\n" +
      "            dispenseCup()\n" +
      "          case _ => unhandledTransition(state.toString, event.toString)\n" +
      "        }\n" +
      "    }\n" +
      "  }\n" +
      "\n" +
      "  private enum State:\n" +
      "    case\n" +
      "    Selecting,\n" +
      "    Brewing\n" +
      "\n" +
      "  private enum Event:\n" +
      "    case\n" +
      "    ChooseDrink,\n" +
      "    Finish\n" +
      "}\n")
  }
}
