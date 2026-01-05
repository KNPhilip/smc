package smc.optimizer

import scala.collection.mutable.{ListBuffer, Set => MutableSet}
import smc.syntaxAnalyzer.{StateMachine, State, Event}

final class Optimizer {
  def optimize(machine: StateMachine): OptimizedStateMachine = {
    val optimized = new OptimizedStateMachine(machine.name, machine.initialState)

    addStates(machine, optimized)
    addEvents(machine, optimized)
    addActions(machine, optimized)
    addTransitions(machine, optimized)

    optimized
  }

  private def addStates(machine: StateMachine, optimized: OptimizedStateMachine): Unit =
    machine.states.filterNot(_.isAbstract).foreach(s => optimized.states += s.name)

  private def addEvents(machine: StateMachine, optimized: OptimizedStateMachine): Unit =
    machine.states.foreach(_.events.foreach(e => optimized.events += e.name))

  private def addActions(machine: StateMachine, optimized: OptimizedStateMachine): Unit =
    machine.states.foreach { s =>
      s.entryActions.foreach(optimized.actions += _)
      s.exitActions.foreach(optimized.actions += _)
      s.events.flatMap(_.actions).foreach(optimized.actions += _)
    }

  private def addTransitions(machine: StateMachine, optimized: OptimizedStateMachine): Unit = {
    val stateMap = machine.states.map(s => s.name -> s).toMap

    machine.states.filterNot(_.isAbstract).foreach { state =>
      val transition = new OptimizedTransition(state.name)
      val usedEvents = MutableSet.empty[String]

      for (hierarchyState <- hierarchyToRoot(state, stateMap))
        addStateTransitions(state, hierarchyState, transition, usedEvents, stateMap)

      optimized.transitions += transition
    }
  }

  private def hierarchyToRoot(state: State, stateMap: Map[String, State]): List[State] = {
    val seen = MutableSet.empty[State]
    val acc = ListBuffer.empty[State]

    def dfs(s: State): Unit =
      if (!seen(s)) {
        seen += s
        acc += s; s.superStates.flatMap(stateMap.get).foreach(dfs)
      }

    dfs(state)
    acc.toList
  }

  private def addStateTransitions(concreteState: State, sourceState: State,
                                  transition: OptimizedTransition, usedEvents: MutableSet[String],
                                  stateMap: Map[String, State]): Unit =
    sourceState.events.foreach { event =>
      if (usedEvents.add(event.name))
        transition.subTransitions += optimizeSubTransition(concreteState, sourceState, event, stateMap)
    }

  private def optimizeSubTransition(concreteState: State, sourceState: State, event: Event,
                                    stateMap: Map[String, State]): OptimizedSubTransition = {
    val target = Option(event.targetState).flatMap(stateMap.get).getOrElse(concreteState)
    val actions = ListBuffer[String]()

    addExitActions(concreteState, target, actions, stateMap)
    addEntryActions(concreteState, target, actions, stateMap)
    actions ++= event.actions

    new OptimizedSubTransition(event.name, target.name, actions)
  }

  private def ancestorsSet(start: State, stateMap: Map[String, State]): Set[State] = {
    val seen = MutableSet.empty[State]

    def dfs(s: State): Unit =
      if (!seen(s)) {
        seen += s
        s.superStates.flatMap(stateMap.get).foreach(dfs)
      }

    dfs(start)
    seen.toSet
  }

  private def depthsFrom(source: State, stateMap: Map[String, State]): Map[State, Int] = {
    val depth = scala.collection.mutable.Map.empty[State, Int]

    def dfs(s: State, d: Int): Unit =
      s.superStates.flatMap(stateMap.get).foreach { p =>
        val nd = d + 1
        if (depth.get(p).forall(_ < nd))
          depth(p) = nd; dfs(p, nd)
      }

    dfs(source, 0)
    depth.toMap
  }

  private def postOrderFromStarts(startNames: Seq[String], stateMap: Map[String, State]): List[State] = {
    val seen = MutableSet.empty[State]
    val res = ListBuffer.empty[State]

    def dfs(s: State): Unit =
      if (!seen(s)) {
        seen += s
        s.superStates.flatMap(stateMap.get).foreach(dfs); res += s
      }

    startNames.flatMap(stateMap.get).foreach(dfs)
    res.toList
  }

  private def addExitActions(source: State, target: State, actions: ListBuffer[String],
                             stateMap: Map[String, State]): Unit = {
    val srcAnc = ancestorsSet(source, stateMap) + source
    val tgtAnc = ancestorsSet(target, stateMap) + target
    val shared = srcAnc intersect tgtAnc

    if (!shared(source))
      actions ++= source.exitActions

    val branchSets = source.superStates.flatMap(stateMap.get).map(s => ancestorsSet(s, stateMap))
    val commonAcrossBranches = branchSets.reduceOption(_ intersect _).getOrElse(Set.empty) -- shared
    val skip = commonAcrossBranches ++ shared
    val seen = MutableSet.empty[State]

    def collectBranch(s: State): Unit =
      if (!seen(s) && !skip(s)) {
        seen += s
        actions ++= s.exitActions
        s.superStates.flatMap(stateMap.get).foreach(collectBranch)
      }

    source.superStates.reverse.flatMap(stateMap.get).foreach(collectBranch)

    val depths = depthsFrom(source, stateMap)
    commonAcrossBranches.toSeq.sortBy(depths.getOrElse(_, 0))(Ordering.Int.reverse)
      .foreach(s => actions ++= s.exitActions)
  }

  private def addEntryActions(source: State, target: State, actions: ListBuffer[String],
                              stateMap: Map[String, State]): Unit = {
    val shared = (ancestorsSet(source, stateMap) + source) intersect (ancestorsSet(target, stateMap) + target)

    postOrderFromStarts(target.superStates.toSeq, stateMap)
      .filterNot(shared)
      .foreach(s => actions ++= s.entryActions)

    if (!shared(target))
      actions ++= target.entryActions
  }
}
