package smc.optimizer

import scala.collection.mutable.ListBuffer

class OptimizedStateMachine(val name: String, val initialState: String) {
  val states: ListBuffer[String] = ListBuffer.empty
  val events: ListBuffer[String] = ListBuffer.empty
  val actions: ListBuffer[String] = ListBuffer.empty
  val transitions: ListBuffer[OptimizedTransition] = ListBuffer.empty
}

class OptimizedTransition(val currentState: String) {
  val subTransitions: ListBuffer[OptimizedSubTransition] = ListBuffer.empty
}

class OptimizedSubTransition(
  val event: String,
  val nextState: String,
  val actions: ListBuffer[String])
