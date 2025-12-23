package smc.syntaxAnalyzer

import scala.collection.mutable.ListBuffer

class StateMachineSyntax {
  var errors: ListBuffer[SyntaxError] = ListBuffer.empty
  var machines: ListBuffer[StateMachine] = ListBuffer.empty

  override def toString: String =
    formatMachines() + formatErrors()

  private def formatMachines(): String =
    if (machines.nonEmpty)
      machines.map(_.toString).mkString("{\n", "", "}\n")
    else
      ""

  private def formatErrors(): String =
    errors.headOption
      .map(e =>
        s"Syntax error: ${e.errorType}. ${e.message}. " +
          s"line ${e.lineNumber}, position ${e.position}.\n"
      )
      .getOrElse("")
}

enum ErrorType:
  case SyntaxError, MachineError, TransitionError,
       SubtransitionError, SuperstateError, EntryExitError

class SyntaxError(val errorType: ErrorType, val message: String,
                  val lineNumber: Int, val position: Int) {
  override def toString: String =
    s"Syntax Error Line: $lineNumber, Position: $position. ($errorType) $message"
}

class StateMachine(val name: String) {
  var initialState: String = null
  var states: ListBuffer[State] = ListBuffer.empty

  override def toString: String = {
    val initial: String = Option(initialState)
      .map(s => s"initial $s").getOrElse("")
    
    s"  machine $name $initial\n" +
      states.map(_.toString).mkString("")
  }
}

class State(var name: String) {
  var events: ListBuffer[Event] = ListBuffer.empty
  var entryActions: ListBuffer[String] = ListBuffer.empty
  var exitActions: ListBuffer[String] = ListBuffer.empty
  var superStates: ListBuffer[String] = ListBuffer.empty
  var isSuperState: Boolean = false

  override def toString: String = {
    val base =
      if (isSuperState) s"($name)" else name

    val state: String = base +
      superStates.map("SU" + _).mkString +
      entryActions.map(" EN" + _).mkString +
      exitActions.map(" EX" + _).mkString

    val events = formatEvents()

    s"    $state $events\n"
  }

  private def formatEvents(): String =
    if (events.size == 1)
      events.head.toString
    else if (events.nonEmpty)
      events.map(e => "      " + e.toString).mkString("{\n", "\n", "\n    }")
    else
      ""
}

class Event(val name: String) {
  var targetState: String = null
  var actions: ListBuffer[String] = ListBuffer.empty

  override def toString: String =
    s"$name ${Option(targetState).getOrElse("")} ${formatActions()}".trim

  private def formatActions(): String =
    if (actions.size == 1)
      actions.head
    else if (actions.nonEmpty)
      actions.mkString("{", " ", "}")
    else
      ""
}
