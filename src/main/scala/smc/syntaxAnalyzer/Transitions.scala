package smc.syntaxAnalyzer

import smc.syntaxAnalyzer.SyntaxEvent.*
import smc.syntaxAnalyzer.SyntaxState.*

final case class Transition(
  currentState: SyntaxState,
  event: SyntaxEvent,
  newState: SyntaxState,
  action: Option[SyntaxBuilder => Unit]
)

val machineSpecTransitions: List[Transition] = List(
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
)

val transitionSpecTransitions: List[Transition] = List(
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
)

val superstateTransitions: List[Transition] = List(
  Transition(MachineSpec, Superstate, SuperstateValue, Some(_.addTransition())),
  Transition(SuperstateValue, Name, SuperstateDeclaration, Some(_.markAsSuperstate())),
  Transition(SuperstateDeclaration, OpenBrace, SubtransitionSpec, Some(_.markAsSubtransition())),
  Transition(EventArrow, OpenBrace, SubtransitionSpec, Some(_.markAsSubtransition())),
)

val entryAndExitTransitions: List[Transition] = List(
  Transition(SubtransitionSpec, Entry, EntryValue, None),
  Transition(EntryValue, Name, SubtransitionSpec, Some(_.setEntryAction())),
  Transition(SubtransitionSpec, Exit, ExitValue, None),
  Transition(ExitValue, Name, SubtransitionSpec, Some(_.setExitAction())),
)

val subtransitionSpecTransitions: List[Transition] = List(
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
