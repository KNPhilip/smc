package smc.syntaxAnalyzer

import scala.collection.mutable.ListBuffer

class StateMachineSyntax {
  var errors: ListBuffer[SyntaxError] = ListBuffer.empty
  var machines: ListBuffer[StateMachine] = ListBuffer.empty

  override def toString: String =
    SyntaxRenderer.render(this)
}

enum ErrorType:
  case SyntaxError, MachineError, TransitionError,
       SubtransitionError, SuperstateError, EntryExitError

class SyntaxError(
  val errorType: ErrorType,
  val message: String,
  val lineNumber: Int,
  val position: Int
)

class StateMachine(val name: String) {
  var initialState: String = null
  var states: ListBuffer[State] = ListBuffer.empty
}

class State(var name: String) {
  var events: ListBuffer[Event] = ListBuffer.empty
  var entryActions: ListBuffer[String] = ListBuffer.empty
  var exitActions: ListBuffer[String] = ListBuffer.empty
  var superStates: ListBuffer[String] = ListBuffer.empty
  var isSuperState: Boolean = false
}

class Event(val name: String) {
  var targetState: String = null
  var actions: ListBuffer[String] = ListBuffer.empty
}

private object SyntaxRenderer {
  def render(syntax: StateMachineSyntax): String =
    renderMachines(syntax.machines) + renderErrors(syntax.errors)

  private def renderMachines(machines: ListBuffer[StateMachine]): String = {
    val rendered =
      machines.collect { case m if m != null => renderMachine(m) }

    if (rendered.nonEmpty)
      rendered.mkString("{\n", "", "}\n")
    else
      ""
  }

  private def renderErrors(errors: ListBuffer[SyntaxError]): String =
    errors.collectFirst {
      case e if e != null =>
        s"Syntax error: ${e.errorType}. ${e.message}. " +
          s"line ${e.lineNumber}, position ${e.position}.\n"
    }.getOrElse("")

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
      if (state.isSuperState) s"(${state.name})" else state.name

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
