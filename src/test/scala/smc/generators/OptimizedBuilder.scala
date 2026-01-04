package smc.generators

import scala.collection.mutable.ListBuffer
import smc.optimizer.{OptimizedStateMachine, OptimizedSubTransition, OptimizedTransition}

object OptimizedBuilder {
  def sm(name: String = "f", initial: String = "I", states: Seq[String],
         events: Seq[String], actions: Seq[String], transitions: Seq[OptimizedTransition]
        ): OptimizedStateMachine = {
    val sm = new OptimizedStateMachine(name, initial)

    sm.states ++= states
    sm.events ++= events
    sm.actions ++= actions
    sm.transitions ++= transitions

    sm
  }

  def transition(state: String, subs: OptimizedSubTransition*): OptimizedTransition = {
    val t = new OptimizedTransition(state)
    t.subTransitions ++= subs
    t
  }

  def sub(event: String, next: String, actions: String*): OptimizedSubTransition =
    new OptimizedSubTransition(event, next, ListBuffer(actions: _*))
}
