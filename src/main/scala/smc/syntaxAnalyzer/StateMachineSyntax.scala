package smc.syntaxAnalyzer

import scala.collection.mutable.ListBuffer

class StateMachineSyntax {
  var errors: ListBuffer[SyntaxError] = ListBuffer.empty
  var machines: ListBuffer[StateMachine] = ListBuffer.empty
}

class SyntaxError {
  var name: String = null
}

class StateMachine {
  var name: String = null
  var initialState: String = null
  var states: ListBuffer[State] = ListBuffer.empty
}

class State {
  var name: String = null
  var events: ListBuffer[Event] = ListBuffer.empty
  var entryActions: ListBuffer[String] = ListBuffer.empty
  var exitActions: ListBuffer[String] = ListBuffer.empty
  var superStates: ListBuffer[String] = ListBuffer.empty
  var isSuperState: Boolean = false
}

class Event {
  var name: String = null
  var targetState: String = null
  var actions: ListBuffer[String] = ListBuffer.empty
}
