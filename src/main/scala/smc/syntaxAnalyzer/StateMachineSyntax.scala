package smc.syntaxAnalyzer

import scala.collection.mutable.ListBuffer

class StateMachineSyntax {
  var errors: ListBuffer[SyntaxError] = ListBuffer.empty
  var machines: ListBuffer[StateMachine] = ListBuffer.empty
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
  var isAbstract: Boolean = false
}

class Event(val name: String) {
  var targetState: String = null
  var actions: ListBuffer[String] = ListBuffer.empty
}
