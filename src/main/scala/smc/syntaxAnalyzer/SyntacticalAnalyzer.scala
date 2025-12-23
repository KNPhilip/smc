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

  private val transitions: List[Transition] =
    machineSpecTransitions ++
    transitionSpecTransitions ++
    superstateTransitions ++
    entryAndExitTransitions ++
    subtransitionSpecTransitions

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
