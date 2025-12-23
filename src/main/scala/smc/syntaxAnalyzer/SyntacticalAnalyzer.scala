package smc.syntaxAnalyzer

import smc.lexicalAnalyzer.TokenCollector

class SyntacticalAnalyzer(builder: SyntaxBuilder) extends TokenCollector {
  private var state: SyntaxState = SyntaxState.MachineSpec

  override def machine(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.Machine, line, position)

  override def initial(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.Initial, line, position)

  override def state(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.State, line, position)

  override def event(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.Event, line, position)

  override def superstate(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.Superstate, line, position)

  override def inherits(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.Inherits, line, position)

  override def entry(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.Entry, line, position)

  override def exit(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.Exit, line, position)

  override def arrow(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.Arrow, line, position)

  override def openBrace(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.OpenBrace, line, position)

  override def closeBrace(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.ClosedBrace, line, position)

  override def dash(line: Int, position: Int): Unit =
    handleEvent(SyntaxEvent.Dash, line, position)

  override def name(line: Int, position: Int, value: String): Unit = {
    builder.setName(value)
    handleEvent(SyntaxEvent.Name, line, position)
  }

  override def error(line: Int, position: Int): Unit =
    builder.syntaxError(line, position)

  private val transitions: List[Transition] = List(
    Transition(SyntaxState.Machine, SyntaxEvent.Machine, SyntaxState.MachineValue, None),
    Transition(SyntaxState.MachineValue, SyntaxEvent.Name, SyntaxState.MachineNamed, Some(_.addMachine())),
    Transition(SyntaxState.MachineNamed, SyntaxEvent.OpenBrace, SyntaxState.MachineSpec, None),
    Transition(SyntaxState.MachineNamed, SyntaxEvent.Arrow, SyntaxState.InitialArrow, None),
    Transition(SyntaxState.InitialArrow, SyntaxEvent.Name, SyntaxState.InitialArrowNamed, Some(_.setInitialState())),
    Transition(SyntaxState.InitialArrowNamed, SyntaxEvent.OpenBrace, SyntaxState.MachineSpec, None),
    Transition(SyntaxState.MachineSpec, SyntaxEvent.Initial, SyntaxState.InitialValue, None),
    Transition(SyntaxState.InitialValue, SyntaxEvent.Name, SyntaxState.MachineSpec, Some(_.setInitialState())),
    Transition(SyntaxState.MachineSpec, SyntaxEvent.ClosedBrace, SyntaxState.Machine, Some(_.concludeStateMachine())),
    Transition(SyntaxState.Machine, SyntaxEvent.End, SyntaxState.Machine, None),
    Transition(SyntaxState.MachineSpec, SyntaxEvent.State, SyntaxState.StateValue, None),
    Transition(SyntaxState.StateValue, SyntaxEvent.OpenBrace, SyntaxState.SubtransitionSpec, Some(_.markAsSubtransition())),
    Transition(SyntaxState.StateValue, SyntaxEvent.Name, SyntaxState.EventArrow, Some(_.addTransition())),
    Transition(SyntaxState.EventArrow, SyntaxEvent.Arrow, SyntaxState.EventValue, None),
    Transition(SyntaxState.EventValue, SyntaxEvent.Name, SyntaxState.NextStateArrow, Some(_.setEvent())),
    Transition(SyntaxState.NextStateArrow, SyntaxEvent.Arrow, SyntaxState.NextStateValue, None),
    Transition(SyntaxState.NextStateValue, SyntaxEvent.Dash, SyntaxState.ActionArrow, Some(_.setEmptyNextState())),
    Transition(SyntaxState.NextStateValue, SyntaxEvent.Name, SyntaxState.ActionArrow, Some(_.setNextState())),
    Transition(SyntaxState.ActionArrow, SyntaxEvent.ClosedBrace, SyntaxState.Machine, Some(_.concludeTransition())),
    Transition(SyntaxState.ActionArrow, SyntaxEvent.Initial, SyntaxState.InitialValue, Some(_.concludeTransition())),
    Transition(SyntaxState.ActionArrow, SyntaxEvent.Superstate, SyntaxState.SuperstateValue, Some(_.concludeTransition())),
    Transition(SyntaxState.ActionArrow, SyntaxEvent.Arrow, SyntaxState.ActionDeclaration, None),
    Transition(SyntaxState.ActionDeclaration, SyntaxEvent.Name, SyntaxState.MachineSpec, Some(_.setActionAndConclude())),
    Transition(SyntaxState.ActionDeclaration, SyntaxEvent.OpenBrace, SyntaxState.ActionValue, None),
    Transition(SyntaxState.ActionValue, SyntaxEvent.Name, SyntaxState.ActionValue, Some(_.addAction())),
    Transition(SyntaxState.ActionValue, SyntaxEvent.ClosedBrace, SyntaxState.MachineSpec, Some(_.concludeTransition())),
    Transition(SyntaxState.MachineSpec, SyntaxEvent.Superstate, SyntaxState.SuperstateValue, Some(_.addTransition())),
    Transition(SyntaxState.SuperstateValue, SyntaxEvent.Name, SyntaxState.SuperstateDeclaration, Some(_.markAsSuperstate())),
    Transition(SyntaxState.SuperstateDeclaration, SyntaxEvent.OpenBrace, SyntaxState.SubtransitionSpec, Some(_.markAsSubtransition())),
    Transition(SyntaxState.EventArrow, SyntaxEvent.OpenBrace, SyntaxState.SubtransitionSpec, Some(_.markAsSubtransition())),
    Transition(SyntaxState.SubtransitionSpec, SyntaxEvent.Entry, SyntaxState.EntryValue, None),
    Transition(SyntaxState.EntryValue, SyntaxEvent.Name, SyntaxState.SubtransitionSpec, Some(_.setEntryAction())),
    Transition(SyntaxState.SubtransitionSpec, SyntaxEvent.Exit, SyntaxState.ExitValue, None),
    Transition(SyntaxState.ExitValue, SyntaxEvent.Name, SyntaxState.SubtransitionSpec, Some(_.setExitAction())),
    Transition(SyntaxState.SubtransitionSpec, SyntaxEvent.Event, SyntaxState.SubeventValue, None),
    Transition(SyntaxState.SubeventValue, SyntaxEvent.Name, SyntaxState.SubNextStateArrow, Some(_.setEvent())),
    Transition(SyntaxState.SubNextStateArrow, SyntaxEvent.Arrow, SyntaxState.SubNextStateValue, None),
    Transition(SyntaxState.SubNextStateValue, SyntaxEvent.Dash, SyntaxState.SubactionArrow, Some(_.setEmptyNextState())),
    Transition(SyntaxState.SubNextStateValue, SyntaxEvent.Name, SyntaxState.SubactionArrow, Some(_.setNextState())),
    Transition(SyntaxState.SubactionArrow, SyntaxEvent.ClosedBrace, SyntaxState.MachineSpec, Some(_.concludeTransition())),
    Transition(SyntaxState.SubactionArrow, SyntaxEvent.Entry, SyntaxState.EntryValue, None),
    Transition(SyntaxState.SubactionArrow, SyntaxEvent.Exit, SyntaxState.ExitValue, None),
    Transition(SyntaxState.SubactionArrow, SyntaxEvent.Arrow, SyntaxState.SubactionDeclaration, None),
    Transition(SyntaxState.SubactionDeclaration, SyntaxEvent.Name, SyntaxState.SubtransitionSpec, Some(_.setActionAndConclude())),
    Transition(SyntaxState.SubactionDeclaration, SyntaxEvent.OpenBrace, SyntaxState.SubactionValue, None),
    Transition(SyntaxState.SubactionValue, SyntaxEvent.Name, SyntaxState.SubactionValue, Some(_.addAction())),
    Transition(SyntaxState.SubactionValue, SyntaxEvent.ClosedBrace, SyntaxState.SubtransitionSpec, None),
    Transition(SyntaxState.SubtransitionSpec, SyntaxEvent.ClosedBrace, SyntaxState.MachineSpec, Some(_.concludeTransition()))
  )

  private def handleEvent(event: SyntaxEvent, line: Int, position: Int): Unit =
    transitions.find(t => t.currentState == state && t.event == event) match {
      case Some(t) =>
        state = t.newState
        t.action.foreach(_(builder))
      case None =>
        handleEventError(event, line, position)
    }

  private def handleEventError(event: SyntaxEvent, line: Int, position: Int): Unit =
    state match {
      case SyntaxState.Machine | SyntaxState.MachineValue | SyntaxState.MachineNamed |
           SyntaxState.InitialArrow | SyntaxState.InitialArrowNamed | SyntaxState.InitialValue =>
        builder.machineError(state, event, line, position)

      case SyntaxState.MachineSpec | SyntaxState.StateValue | SyntaxState.EventArrow |
           SyntaxState.EventValue | SyntaxState.NextStateArrow | SyntaxState.NextStateValue |
           SyntaxState.ActionArrow | SyntaxState.ActionDeclaration | SyntaxState.ActionValue =>
        builder.transitionError(state, event, line, position)

      case SyntaxState.SuperstateValue | SyntaxState.SuperstateDeclaration =>
        builder.superstateError(state, event, line, position)

      case SyntaxState.EntryValue | SyntaxState.ExitValue =>
        builder.entryExitError(state, event, line, position)

      case SyntaxState.SubtransitionSpec | SyntaxState.SubeventValue | SyntaxState.SubNextStateArrow |
           SyntaxState.SubNextStateValue | SyntaxState.SubactionArrow | SyntaxState.SubactionDeclaration |
           SyntaxState.SubactionValue =>
        builder.subtransitionError(state, event, line, position)
    }
}
