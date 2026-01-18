package smc.semanticAnalyzer

import scala.collection.mutable.ListBuffer
import smc.semanticAnalyzer.SemanticError.*
import smc.syntaxAnalyzer.{StateMachineSyntax, StateMachine, State}

enum SemanticError:
  case
  NO_MACHINES,
  DUPLICATE_MACHINE,
  NO_INITIAL_STATE,
  NO_TRANSITIONS,
  UNDEFINED_INITIAL_STATE,
  UNDEFINED_NEXT_STATE,
  UNUSED_STATE,
  DUPLICATE_STATE,
  DUPLICATE_TRANSITION,
  UNDEFINED_SUPER_STATE,
  SUPERSTATE_AS_NEXT_STATE,
  SUPERSTATE_CONFLICT,
  DUPLICATE_NAME

final class SemanticAnalyzer {
  var errors: ListBuffer[SemanticError] = ListBuffer.empty[SemanticError]

  def analyze(input: StateMachineSyntax): ListBuffer[SemanticError] = {
    errors = ListBuffer.empty[SemanticError]
    checkForErrors(input)
    errors
  }

  private def checkForErrors(input: StateMachineSyntax): Unit = {
    checkForNoMachines(input)
    checkForDuplicateMachines(input)
    input.machines.foreach(analyzeMachine)
  }

  private def checkForNoMachines(input: StateMachineSyntax): Unit =
    if (input.machines.isEmpty)
      errors += NO_MACHINES

  private def checkForDuplicateMachines(input: StateMachineSyntax): Unit =
    if (input.machines.map(_.name).toSet.size != input.machines.size)
      errors += DUPLICATE_MACHINE

  private def analyzeMachine(input: StateMachine): Unit = {
    checkForNoInitialState(input)
    checkForNoTransitions(input)
    checkForUndefinedInitialState(input)
    analyzeStates(input.states.toList, input.initialState)
  }

  private def checkForNoInitialState(input: StateMachine): Unit =
    if (input.initialState == null)
      errors += NO_INITIAL_STATE

  private def checkForNoTransitions(input: StateMachine): Unit =
    if (input.states.isEmpty)
      errors += NO_TRANSITIONS

  private def checkForUndefinedInitialState(input: StateMachine): Unit =
    if (!input.states.map(_.name).contains(input.initialState))
      errors += UNDEFINED_INITIAL_STATE

  private def analyzeStates(input: List[State], initialState: String): Unit = {
    checkForUndefinedSuperstate(input)
    checkForSuperstatesAsNextState(input)
    checkForConflictInSuperstate(input)
    checkForUndefinedNextState(input)
    checkForDuplicateStates(input)
    checkForDuplicateTransitions(input)
    checkForUnusedStates(input, initialState)
    checkForDuplicateNames(input)
  }

  private def checkForUndefinedSuperstate(input: List[State]): Unit = {
    val superstates: List[String] = input
      .filter(state => state.isAbstract)
      .map(_.name)

    input.foreach(state => {
      state.superStates.foreach(superstate => {
        if (!superstates.contains(superstate))
          errors += UNDEFINED_SUPER_STATE
      })
    })
  }

  private def checkForSuperstatesAsNextState(input: List[State]): Unit = {
    val superstates: List[String] = input
      .filter(state => state.isAbstract)
      .map(_.name)

    input.foreach(state => {
      val nextStates = state.events.map(_.targetState)

      nextStates.foreach(nextState => {
        if (superstates.contains(nextState))
          errors += SUPERSTATE_AS_NEXT_STATE
      })
    })
  }

  private def checkForConflictInSuperstate(input: List[State]): Unit = {
    val superstates = input.filter(state => state.isAbstract)
    val relevantStates = input.filter(state => state.superStates.nonEmpty)

    relevantStates.foreach(state => {
      state.superStates.foreach { superstateName =>
        superstates.find(_.name == superstateName).foreach { superstate =>
          if (hasConflictingNextState(superstate, state))
            errors += SUPERSTATE_CONFLICT
        }
      }
    })
  }

  private def hasConflictingNextState(superstate: State, state: State): Boolean = {
    val superTransitions: Map[String, String] =
      superstate.events
        .filter(_.targetState != null)
        .map(e => e.name -> e.targetState)
        .toMap

    state.events.exists { subEvent =>
      superTransitions.get(subEvent.name) match {
        case Some(superTarget) =>
          superTarget != subEvent.targetState
        case None =>
          false
      }
    }
  }

  private def checkForUndefinedNextState(input: List[State]): Unit = {
    val stateNames = input.map(_.name).toSet

    val allDefined = input.forall { state =>
      state.events.forall { event =>
        event.targetState != null && stateNames.contains(event.targetState)
      }
    }

    if (!allDefined)
      errors += UNDEFINED_NEXT_STATE
  }

  private def checkForDuplicateStates(input: List[State]): Unit = {
    val stateNames: List[String] = input.map(_.name)

    if (stateNames.toSet.size != input.length)
      errors += DUPLICATE_STATE
  }

  private def checkForDuplicateTransitions(states: List[State]): Unit = {
    val superstates: Map[String, State] = states
      .filter(_.isAbstract)
      .map(s => s.name -> s)
      .toMap

    states.foreach { state =>
      val inheritedEvents: List[String] = state.superStates.toList.flatMap { superName =>
        superstates.get(superName)
          .map(_.events.toList.map(_.name))
          .getOrElse(Nil)
      }
      val allEventNames: List[String] = state.events.toList.map(_.name) ++ inheritedEvents

      if (allEventNames.toSet.size != allEventNames.size)
        errors += DUPLICATE_TRANSITION
    }
  }

  private def checkForUnusedStates(states: List[State], initialState: String): Unit = {
    val stateNames = states.map(_.name).toSet
    val explicitlyTargeted: Set[String] = states
      .flatMap(_.events.map(_.targetState))
      .filter(_ != null)
      .toSet

    val superstatesWithChildren: Set[String] =
      states.flatMap(_.superStates).toSet

    val unused = stateNames.diff(
      explicitlyTargeted ++ superstatesWithChildren ++ Option(initialState).toSet)

    if (unused.nonEmpty)
      errors += UNUSED_STATE
  }

  private def checkForDuplicateNames(states: List[State]): Unit = {
    import smc.toCamelCase
    
    val eventNames: List[String] = states.flatMap(_.events.map(_.name))
    
    val transitionActions: List[String] = states.flatMap(_.events.flatMap(_.actions))
    val entryActions: List[String] = states.flatMap(_.entryActions)
    val exitActions: List[String] = states.flatMap(_.exitActions)
    val allActions: List[String] = transitionActions ++ entryActions ++ exitActions
    
    // Normalize to camelCase to detect conflicts after code generation
    val normalizedEvents = eventNames.map(_.toCamelCase).toSet
    val normalizedActions = allActions.map(_.toCamelCase).toSet
    
    val eventActionConflict = normalizedEvents.intersect(normalizedActions)
    
    if (eventActionConflict.nonEmpty)
      errors += DUPLICATE_NAME
  }
}
