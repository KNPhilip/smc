package smc.generators.selection

import munit.FunSuite
import scala.compiletime.uninitialized
import smc.generators.OptimizedBuilder._
import smc.optimizer.{OptimizedStateMachine, OptimizedSubTransition, OptimizedTransition}

abstract class SelectionFactorySuite extends FunSuite {
  protected var visitor: SelectionNodeVisitor = uninitialized
  protected val output = new StringBuilder

  override def beforeEach(context: BeforeEach): Unit = {
    visitor = null
    output.clear()
  }

  protected def withVisitor(v: (String => Unit) => SelectionNodeVisitor): Unit =
    visitor = v(output.append)

  protected def assertGenerated(machine: OptimizedStateMachine, expected: String): Unit = {
    SelectionNodeFactory.generate(machine).accept(visitor)
    assertEquals(output.toString(), expected)
  }
}

abstract class DummyNscVisitor extends SelectionNodeVisitor {
  override def visit(node: SwitchCaseNode): Unit = ()
  override def visit(node: CaseNode): Unit = ()
  override def visit(node: DefaultCaseNode): Unit = ()
  override def visit(node: FunctionCallNode): Unit = ()
  override def visit(node: EnumNode): Unit = ()
  override def visit(node: NextStateNode): Unit = ()
  override def visit(node: StatePropertyNode): Unit = ()
  override def visit(node: EventDelegatorsNode): Unit = ()

  override def visit(node: HandleEventNode): Unit =
    node.switchCase.accept(this)

  override def visit(node: FsmClassNode): Unit = {
    node.delegators.accept(this)
    node.stateEnum.accept(this)
    node.eventEnum.accept(this)
    node.stateProperty.accept(this)
    node.handleEvent.accept(this)
  }

  override def writeFiles(path: String, fileName: String): Unit = ()
}

final class TestVisitor(out: String => Unit) extends DummyNscVisitor {
  override def visit(node: SwitchCaseNode): Unit = {
    out(s"s ${node.variableName} {")
    node.caseNodes.foreach(_.accept(this))
    out("}")
  }

  override def visit(node: CaseNode): Unit = {
    out(s"case ${node.caseName} {")
    node.caseActionNode.foreach(_.accept(this))
    out("}")
  }

  override def visit(node: FunctionCallNode): Unit = {
    out(s"${node.functionName}(")
    node.argument.foreach(_.accept(this))
    out(") ")
  }

  override def visit(node: NextStateNode): Unit =
    out(s"State.${node.nextState}")

  override def visit(node: DefaultCaseNode): Unit =
    out(s" default(${node.state});")
}

final class EnumVisitor(out: String => Unit) extends DummyNscVisitor {
  override def visit(node: EnumNode): Unit =
    out(s"enum ${node.name} ${node.enumerators.mkString("[", ", ", "]")} ")
}

final class StatePropertyVisitor(out: String => Unit) extends DummyNscVisitor {
  override def visit(node: StatePropertyNode): Unit =
    out(s"state property = ${node.initialState}")
}

final class EventDelegatorVisitor(out: String => Unit) extends DummyNscVisitor {
  override def visit(node: EventDelegatorsNode): Unit =
    out(s"delegators ${node.events}")
}

final class HandleEventVisitor(out: String => Unit) extends DummyNscVisitor {
  override def visit(node: SwitchCaseNode): Unit = out("s")
  override def visit(node: HandleEventNode): Unit = {
    out("he(")
    node.switchCase.accept(this)
    out(")")
  }
}

final class FsmClassVisitor(out: String => Unit) extends DummyNscVisitor {
  override def visit(node: SwitchCaseNode): Unit = out("sc")
  override def visit(node: EnumNode): Unit = out("e ")
  override def visit(node: StatePropertyNode): Unit = out("p ")
  override def visit(node: EventDelegatorsNode): Unit = out("d ")
  override def visit(node: HandleEventNode): Unit = out("he ")
  override def visit(node: FsmClassNode): Unit = {
    out(s"class ${node.className} {")
    node.delegators.accept(this)
    node.stateEnum.accept(this)
    node.eventEnum.accept(this)
    node.stateProperty.accept(this)
    node.handleEvent.accept(this)
    node.handleEvent.switchCase.accept(this)
    out("}")
  }
}

final class SelectionGenerationSuite extends SelectionFactorySuite {
  private def singleStateMachine = sm(
    states = Seq("I"),
    events = Seq("e"),
    actions = Seq("a"),
    transitions = Seq(transition("I", sub("e", "I", "a"))))

  test("Empty transitions produces empty state switch") {
    withVisitor(new TestVisitor(_))
    val machine = sm(states = Seq("I"), events = Seq("e"), actions = Seq("a"), transitions = Seq.empty)
    assertGenerated(machine, "s state {}")
  }

  test("Single transition works") {
    withVisitor(new TestVisitor(_))
    assertGenerated(
      singleStateMachine,
      "s state {case I {s event {case e {setState(State.I) a() } default(I);}}}")
  }

  test("Two transitions works") {
    withVisitor(new TestVisitor(_))

    val machine = sm(
      states = Seq("I", "S"),
      events = Seq("e1", "e2"),
      actions = Seq("a1", "a2"),
      transitions = Seq(
        transition("I", sub("e1", "S", "a1")),
        transition("S", sub("e2", "I", "a2"))))

    assertGenerated(
      machine,
      "s state {case I {s event {case e1 {setState(State.S) a1() } default(I);}}case S {s event {case e2 {setState(State.I) a2() } default(S);}}}"
    )
  }

  test("Generation of enums") {
    withVisitor(new EnumVisitor(_))

    val machine = sm(
      states = Seq("I", "S"),
      events = Seq("e1", "e2"),
      actions = Seq("a1", "a2"),
      transitions = Seq.empty)

    assertGenerated(machine, "enum State [I, S] enum Event [e1, e2] ")
  }

  test("Generation of state property") {
    withVisitor(new StatePropertyVisitor(_))
    assertGenerated(singleStateMachine, "state property = I")
  }

  test("Generation of event delegators") {
    withVisitor(new EventDelegatorVisitor(_))

    val machine = sm(
      states = Seq("I", "S"),
      events = Seq("e1", "e2"),
      actions = Seq("a1", "a2"),
      transitions = Seq.empty)

    assertGenerated(machine, "delegators List(e1, e2)")
  }

  test("Generation of handle event") {
    withVisitor(new HandleEventVisitor(_))
    assertGenerated(singleStateMachine, "he(s)")
  }

  test("Generation of FSM class node") {
    withVisitor(new FsmClassVisitor(_))

    val machine = sm(
      actions = Seq("acts"),
      states = Seq("I"),
      events = Seq("e"),
      transitions = Seq(
        transition("I", sub("e", "I", "a"))))

    assertGenerated(machine, "class f {d e e p he sc}")
  }

  test("State order is preserved") {
    withVisitor(new TestVisitor(_))

    val machine = sm(
      states = Seq("S2", "S1"),
      events = Seq("e"),
      actions = Seq("a"),
      transitions = Seq(
        transition("S2", sub("e", "S1", "a")),
        transition("S1", sub("e", "S2", "a"))))

    assertGenerated(
      machine,
      "s state {case S2 {s event {case e {setState(State.S1) a() } default(S2);}}case S1 {s event {case e {setState(State.S2) a() } default(S1);}}}"
    )
  }

  test("Multiple actions are generated in order") {
    withVisitor(new TestVisitor(_))

    val machine = sm(
      states = Seq("I"),
      events = Seq("e"),
      actions = Seq("a1", "a2", "a3"),
      transitions = Seq(
        transition("I", sub("e", "I", "a1", "a2", "a3"))))

    assertGenerated(
      machine,
      "s state {case I {s event {case e {setState(State.I) a1() a2() a3() } default(I);}}}"
    )
  }

  test("Default case uses current state") {
    withVisitor(new TestVisitor(_))

    val machine = sm(
      states = Seq("A"),
      events = Seq("e"),
      actions = Seq("a"),
      transitions = Seq(
        transition("A", sub("e", "B", "a"))))

    assertGenerated(
      machine,
      "s state {case A {s event {case e {setState(State.B) a() } default(A);}}}"
    )
  }
}
