package smc.syntaxAnalyzer

import smc.lexicalAnalyzer.TokenCollector
import smc.syntaxAnalyzer.SyntaxState.*
import smc.syntaxAnalyzer.SyntaxEvent.*

class SyntacticalAnalyzer(builder: SyntaxBuilder) extends TokenCollector {
  private var state: SyntaxState = MachineSpec

  override def machine(line: Int, position: Int): Unit =
    handleEvent(Machine, line, position)

  override def initial(line: Int, position: Int): Unit =
    handleEvent(Initial, line, position)

  override def state(line: Int, position: Int): Unit =
    handleEvent(State, line, position)

  override def event(line: Int, position: Int): Unit =
    handleEvent(Event, line, position)

  override def superstate(line: Int, position: Int): Unit =
    handleEvent(Superstate, line, position)

  override def inherits(line: Int, position: Int): Unit =
    handleEvent(Inherits, line, position)

  override def entry(line: Int, position: Int): Unit =
    handleEvent(Entry, line, position)

  override def exit(line: Int, position: Int): Unit =
    handleEvent(Exit, line, position)

  override def arrow(line: Int, position: Int): Unit =
    handleEvent(Arrow, line, position)

  override def openBrace(line: Int, position: Int): Unit =
    handleEvent(OpenBrace, line, position)

  override def closeBrace(line: Int, position: Int): Unit =
    handleEvent(ClosedBrace, line, position)

  override def dash(line: Int, position: Int): Unit =
    handleEvent(Dash, line, position)

  override def name(line: Int, position: Int, value: String): Unit = {
    builder.setName(value)
    handleEvent(Name, line, position)
  }

  override def error(line: Int, position: Int): Unit =
    builder.syntaxError(line, position)

  private val transitions: List[Transition] = List(
    Transition(MachineDeclaration, Machine, MachineValue, None),
    Transition(MachineValue, Name, MachineNamed, Some(_.addMachine())),
    Transition(MachineNamed, OpenBrace, MachineSpec, None),
    Transition(MachineNamed, Arrow, InitialArrow, None),
    Transition(InitialArrow, Name, InitialArrowNamed, Some(_.setInitialState())),
    Transition(InitialArrowNamed, OpenBrace, MachineSpec, None),
    Transition(MachineSpec, Initial, InitialValue, None),
    Transition(InitialValue, Name, MachineSpec, Some(_.setInitialState())),
    Transition(MachineSpec, ClosedBrace, MachineDeclaration, Some(_.concludeStateMachine())),
    Transition(MachineDeclaration, End, MachineDeclaration, None),
    Transition(MachineSpec, State, StateValue, None),
    Transition(StateValue, OpenBrace, SubtransitionSpec, Some(_.markAsSubtransition())),
    Transition(StateValue, Name, EventArrow, Some(_.addTransition())),
    Transition(EventArrow, Arrow, EventValue, None),
    Transition(EventValue, Name, NextStateArrow, Some(_.setEvent())),
    Transition(NextStateArrow, Arrow, NextStateValue, None),
    Transition(NextStateValue, Dash, ActionArrow, Some(_.setEmptyNextState())),
    Transition(NextStateValue, Name, ActionArrow, Some(_.setNextState())),
    Transition(ActionArrow, ClosedBrace, MachineDeclaration, Some(_.concludeTransition())),
    Transition(ActionArrow, Initial, InitialValue, Some(_.concludeTransition())),
    Transition(ActionArrow, Superstate, SuperstateValue, Some(_.concludeTransition())),
    Transition(ActionArrow, Arrow, ActionDeclaration, None),
    Transition(ActionDeclaration, Name, MachineSpec, Some(_.setActionAndConclude())),
    Transition(ActionDeclaration, OpenBrace, ActionValue, None),
    Transition(ActionValue, Name, ActionValue, Some(_.addAction())),
    Transition(ActionValue, ClosedBrace, MachineSpec, Some(_.concludeTransition())),
    Transition(MachineSpec, Superstate, SuperstateValue, Some(_.addTransition())),
    Transition(SuperstateValue, Name, SuperstateDeclaration, Some(_.markAsSuperstate())),
    Transition(SuperstateDeclaration, OpenBrace, SubtransitionSpec, Some(_.markAsSubtransition())),
    Transition(EventArrow, OpenBrace, SubtransitionSpec, Some(_.markAsSubtransition())),
    Transition(SubtransitionSpec, Entry, EntryValue, None),
    Transition(EntryValue, Name, SubtransitionSpec, Some(_.setEntryAction())),
    Transition(SubtransitionSpec, Exit, ExitValue, None),
    Transition(ExitValue, Name, SubtransitionSpec, Some(_.setExitAction())),
    Transition(SubtransitionSpec, Event, SubeventValue, None),
    Transition(SubeventValue, Name, SubNextStateArrow, Some(_.setEvent())),
    Transition(SubNextStateArrow, Arrow, SubNextStateValue, None),
    Transition(SubNextStateValue, Dash, SubactionArrow, Some(_.setEmptyNextState())),
    Transition(SubNextStateValue, Name, SubactionArrow, Some(_.setNextState())),
    Transition(SubactionArrow, ClosedBrace, MachineSpec, Some(_.concludeTransition())),
    Transition(SubactionArrow, Entry, EntryValue, None),
    Transition(SubactionArrow, Exit, ExitValue, None),
    Transition(SubactionArrow, Arrow, SubactionDeclaration, None),
    Transition(SubactionDeclaration, Name, SubtransitionSpec, Some(_.setActionAndConclude())),
    Transition(SubactionDeclaration, OpenBrace, SubactionValue, None),
    Transition(SubactionValue, Name, SubactionValue, Some(_.addAction())),
    Transition(SubactionValue, ClosedBrace, SubtransitionSpec, None),
    Transition(SubtransitionSpec, ClosedBrace, MachineSpec, Some(_.concludeTransition()))
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
      case MachineDeclaration | MachineValue | MachineNamed | InitialArrow |
           InitialArrowNamed | InitialValue =>
        builder.machineError(state, event, line, position)

      case MachineSpec | StateValue | EventArrow | EventValue | NextStateArrow |
           NextStateValue | ActionArrow | ActionDeclaration | ActionValue =>
        builder.transitionError(state, event, line, position)

      case SuperstateValue | SuperstateDeclaration =>
        builder.superstateError(state, event, line, position)

      case EntryValue | ExitValue =>
        builder.entryExitError(state, event, line, position)

      case SubtransitionSpec | SubeventValue | SubNextStateArrow |
           SubNextStateValue | SubactionArrow | SubactionDeclaration | SubactionValue =>
        builder.subtransitionError(state, event, line, position)
    }
}
