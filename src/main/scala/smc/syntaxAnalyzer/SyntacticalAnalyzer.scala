package smc.syntaxAnalyzer

import smc.lexicalAnalyzer.TokenCollector
import smc.syntaxAnalyzer.ErrorType.*
import smc.syntaxAnalyzer.SyntaxState.*
import smc.syntaxAnalyzer.SyntaxEvent.*

final class SyntacticalAnalyzer extends TokenCollector {
  private var state: SyntaxState = MachineDeclaration
  private val builder: SyntaxBuilder = new SyntaxBuilder()

  override def machine(line: Int, position: Int): Unit =
    handleEvent(Machine, line, position)

  override def initial(line: Int, position: Int): Unit =
    handleEvent(Initial, line, position)

  override def state(line: Int, position: Int): Unit =
    handleEvent(State, line, position)

  override def event(line: Int, position: Int): Unit =
    handleEvent(Event, line, position)

  override def abstractState(line: Int, position: Int): Unit =
    handleEvent(Abstract, line, position)

  override def colon(line: Int, position: Int): Unit =
    handleEvent(Colon, line, position)

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

  def getStateMachineSyntax: StateMachineSyntax = builder.getStateMachine

  def clear(): Unit = {
    state = MachineDeclaration
    builder.clear()
  }

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
        builder.tableError(MachineError, state, event, line, position)

      case MachineSpec | StateValue | EventArrow | EventValue | NextStateArrow |
           NextStateValue | ActionArrow | ActionDeclaration | ActionValue =>
        builder.tableError(TransitionError, state, event, line, position)

      case InheritsValue | SuperstateValue | SuperstateDeclaration =>
        builder.tableError(SuperstateError, state, event, line, position)

      case EntryDeclaration | EntryValue | ExitValue | ExitDeclaration =>
        builder.tableError(EntryExitError, state, event, line, position)

      case SubtransitionSpec | SubeventValue | SubNextStateArrow |
           SubNextStateValue | SubactionArrow | SubactionDeclaration | SubactionValue =>
        builder.tableError(SubtransitionError, state, event, line, position)
    }

  private lazy val transitions: List[Transition] =
    machineSpecTransitions ++
    transitionSpecTransitions ++
    superstateTransitions ++
    entryAndExitTransitions ++
    subtransitionSpecTransitions

  private val machineSpecTransitions: List[Transition] = List(
    Transition(MachineDeclaration, Machine, MachineValue, None),
    Transition(MachineValue, Name, MachineNamed, Some(_.addMachine())),
    Transition(MachineNamed, OpenBrace, MachineSpec, None),
    Transition(MachineNamed, Arrow, InitialArrow, None),
    Transition(InitialArrow, Name, InitialArrowNamed, Some(_.setInitialState())),
    Transition(InitialArrowNamed, OpenBrace, MachineSpec, None),
    Transition(MachineSpec, Initial, InitialValue, None),
    Transition(InitialValue, Name, MachineSpec, Some(_.setInitialState())),
    Transition(MachineSpec, ClosedBrace, MachineDeclaration, Some(_.concludeStateMachine()))
  )

  private val transitionSpecTransitions: List[Transition] = List(
    Transition(MachineSpec, State, StateValue, None),
    Transition(StateValue, Name, EventArrow, Some(_.addTransition())),
    Transition(EventArrow, OpenBrace, SubtransitionSpec, None),
    Transition(EventArrow, Colon, InheritsValue, None),
    Transition(InheritsValue, Name, InheritsValue, Some(_.addInheritance())),
    Transition(InheritsValue, OpenBrace, SubtransitionSpec, None),
    Transition(EventArrow, Arrow, EventValue, None),
    Transition(EventValue, Name, NextStateArrow, Some(_.setEvent())),
    Transition(NextStateArrow, Arrow, NextStateValue, None),
    Transition(NextStateValue, Dash, ActionArrow, Some(_.setEmptyNextState())),
    Transition(NextStateValue, Name, ActionArrow, Some(_.setNextState())),
    Transition(ActionArrow, ClosedBrace, MachineDeclaration, Some(_.concludeStateMachine())),
    Transition(ActionArrow, State, StateValue, Some(_.concludeTransition())),
    Transition(ActionArrow, Initial, InitialValue, Some(_.concludeTransition())),
    Transition(ActionArrow, Abstract, SuperstateValue, Some(_.concludeTransition())),
    Transition(ActionArrow, Arrow, ActionDeclaration, None),
    Transition(ActionDeclaration, Name, MachineSpec, Some(_.addAction())),
    Transition(ActionDeclaration, OpenBrace, ActionValue, None),
    Transition(ActionValue, Name, ActionValue, Some(_.addAction())),
    Transition(ActionValue, ClosedBrace, MachineSpec, Some(_.concludeTransition()))
  )

  private val superstateTransitions: List[Transition] = List(
    Transition(MachineSpec, Abstract, SuperstateValue, Some(_.addTransition())),
    Transition(SuperstateValue, Name, SuperstateDeclaration, Some(_.markAsSuperstate())),
    Transition(SuperstateDeclaration, Colon, InheritsValue, None),
    Transition(SuperstateDeclaration, OpenBrace, SubtransitionSpec, None),
    Transition(EventArrow, OpenBrace, SubtransitionSpec, None)
  )

  private val entryAndExitTransitions: List[Transition] = List(
    Transition(SubtransitionSpec, Entry, EntryDeclaration, None),
    Transition(EntryDeclaration, Name, SubtransitionSpec, Some(_.addEntryAction())),
    Transition(EntryDeclaration, OpenBrace, EntryValue, None),
    Transition(EntryValue, Name, EntryValue, Some(_.addEntryAction())),
    Transition(EntryValue, ClosedBrace, SubtransitionSpec, None),
    Transition(SubtransitionSpec, Exit, ExitDeclaration, None),
    Transition(ExitDeclaration, Name, SubtransitionSpec, Some(_.addExitAction())),
    Transition(ExitDeclaration, OpenBrace, ExitValue, None),
    Transition(ExitValue, Name, ExitValue, Some(_.addExitAction())),
    Transition(ExitValue, ClosedBrace, SubtransitionSpec, None)
  )

  private val subtransitionSpecTransitions: List[Transition] = List(
    Transition(SubtransitionSpec, Event, SubeventValue, None),
    Transition(SubeventValue, Name, SubNextStateArrow, Some(_.setEvent())),
    Transition(SubNextStateArrow, Arrow, SubNextStateValue, None),
    Transition(SubNextStateValue, Dash, SubactionArrow, Some(_.setEmptyNextState())),
    Transition(SubNextStateValue, Name, SubactionArrow, Some(_.setNextState())),
    Transition(SubactionArrow, ClosedBrace, MachineSpec, Some(_.concludeTransition())),
    Transition(SubactionArrow, Event, SubeventValue, None),
    Transition(SubactionArrow, Entry, EntryDeclaration, None),
    Transition(SubactionArrow, Exit, ExitDeclaration, None),
    Transition(SubactionArrow, Arrow, SubactionDeclaration, None),
    Transition(SubactionDeclaration, Name, SubtransitionSpec, Some(_.addAction())),
    Transition(SubactionDeclaration, OpenBrace, SubactionValue, None),
    Transition(SubactionValue, Name, SubactionValue, Some(_.addAction())),
    Transition(SubactionValue, ClosedBrace, SubtransitionSpec, None),
    Transition(SubtransitionSpec, ClosedBrace, MachineSpec, Some(_.concludeTransition()))
  )

  private final case class Transition(
    currentState: SyntaxState,
    event: SyntaxEvent,
    newState: SyntaxState,
    action: Option[SyntaxBuilder => Unit]
  )
}
