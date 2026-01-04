package smc.generators.selection

import smc.optimizer.{OptimizedStateMachine, OptimizedTransition, OptimizedSubTransition}

object SelectionNodeFactory {
  def generate(sm: OptimizedStateMachine): SelectionNode = {
    val eventDelegators = EventDelegatorsNode(sm.events.toList)
    val stateProperty = StatePropertyNode(sm.initialState)
    val stateEnum = EnumNode("State", sm.states.toList)
    val eventEnum = EnumNode("Event", sm.events.toList)
    val stateSwitch = buildStateSwitch(sm)
    val handleEvent = HandleEventNode(stateSwitch)

    FsmClassNode(
      className     = sm.name,
      delegators    = eventDelegators,
      eventEnum     = eventEnum,
      stateEnum     = stateEnum,
      stateProperty = stateProperty,
      handleEvent   = handleEvent,
      actions       = sm.actions.toList,
      states        = sm.states.toList
    )
  }

  private def buildStateSwitch(sm: OptimizedStateMachine): SwitchCaseNode = {
    val stateCases = sm.transitions.map(buildStateCase)
    SwitchCaseNode(variableName = "state", caseNodes = stateCases.toList)
  }

  private def buildStateCase(t: OptimizedTransition): CaseNode = {
    val eventSwitch = buildEventSwitch(t)
    CaseNode(switchName = "State", caseName = t.currentState, caseActionNode = Some(eventSwitch))
  }

  private def buildEventSwitch(t: OptimizedTransition): SwitchCaseNode = {
    val eventCases = t.subTransitions
      .map(buildEventCase) :+ DefaultCaseNode(t.currentState)

    SwitchCaseNode(variableName = "event", caseNodes = eventCases.toList)
  }

  private def buildEventCase(st: OptimizedSubTransition): CaseNode = {
    val actions = buildActions(st)
    CaseNode(switchName = "Event", caseName = st.event, caseActionNode = Some(actions))
  }

  private def buildActions(st: OptimizedSubTransition): CompositeNode = {
    val setState = buildSetStateNode(st.nextState)
    val actionCalls = st.actions.toList.map(FunctionCallNode(_))
    CompositeNode(nodes = setState :: actionCalls)
  }

  private def buildSetStateNode(stateName: String): FunctionCallNode = {
    val nextStateNode = NextStateNode(stateName)
    FunctionCallNode(functionName = "setState", argument = Some(nextStateNode))
  }
}
