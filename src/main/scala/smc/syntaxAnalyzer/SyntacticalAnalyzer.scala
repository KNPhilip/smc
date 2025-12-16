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
    handleEvent(SyntaxEvent.Name, line, position)
  }

  override def error(line: Int, position: Int): Unit = {

  }

  private val transitions: List[Transition] = List(
    Transition(SyntaxState.Machine, SyntaxEvent.Machine, SyntaxState.MachineValue, None),
    Transition(SyntaxState.MachineValue, SyntaxEvent.Name, SyntaxState.MachineNamed, Some(_.newMachine())),
    Transition(SyntaxState.MachineNamed, SyntaxEvent.OpenBrace, SyntaxState.MachineSpec, None),
    Transition(SyntaxState.MachineNamed, SyntaxEvent.Arrow, SyntaxState.InitialArrow, None),
    Transition(SyntaxState.InitialArrow, SyntaxEvent.Name, SyntaxState.InitialArrowNamed, Some(_.setInitialState())),
    Transition(SyntaxState.InitialArrowNamed, SyntaxEvent.OpenBrace, SyntaxState.MachineSpec, None)
  )

  private def handleEvent(event: SyntaxEvent, line: Int, position: Int): Unit =
    transitions.find(t => t.currentState == state && t.event == event) match {
      case Some(t) =>
        state = t.newState
        t.action.foreach(_(builder))

      case None =>
        {}
    }
}
