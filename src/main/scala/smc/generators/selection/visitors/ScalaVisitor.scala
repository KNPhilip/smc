package smc.generators.selection.visitors

import smc.generators.selection._

import java.io.IOException
import java.nio.file.{FileSystems, Files}

final class ScalaVisitor extends SelectionNodeVisitor {
  private val output = new StringBuilder
  private var states: List[String] = List.empty

  override def visit(node: FsmClassNode): Unit = {
    output.append(s"abstract class ${node.className} {\n")
    output.append(s"${indent(1)}def unhandledTransition(state: String, event: String): Unit\n\n")

    node.actions.foreach { action =>
      output.append(s"${indent(1)}protected def $action(): Unit\n\n")
    }

    states = node.stateEnum.enumerators
    node.delegators.accept(this)
    node.stateProperty.accept(this)
    node.handleEvent.accept(this)
    node.stateEnum.accept(this)
    node.eventEnum.accept(this)

    output.append("}\n")
  }

  override def visit(node: EventDelegatorsNode): Unit =
    node.events.foreach { event =>
      output.append(
        s"${indent(1)}def $event(): Unit =\n" +
        s"${indent(2)}handleEvent(Event.$event)\n\n")
    }

  override def visit(node: StatePropertyNode): Unit = {
    output.append(s"${indent(1)}private var state: State = State.${node.initialState}\n\n")
    output.append(
      s"${indent(1)}private def setState(s: State): Unit =\n" +
      s"${indent(2)}state = s\n\n")
  }

  override def visit(node: HandleEventNode): Unit = {
    output.append(s"${indent(1)}private def handleEvent(event: Event): Unit = {\n")
    node.switchCase.accept(this)
    output.append(s"${indent(1)}}\n")
  }

  override def visit(node: SwitchCaseNode): Unit = {
    if (node.variableName == "state") {
      output.append(s"${indent(2)}${node.variableName} match {\n")
      node.generateCases(this)
      output.append(s"${indent(2)}}\n")
    } else {
      output.append(s"${indent(4)}${node.variableName} match {\n")
      node.generateCases(this)
      output.append(s"${indent(4)}}\n")
    }
  }

  override def visit(node: CaseNode): Unit = {
    if (states.contains(node.caseName)) {
      output.append(s"${indent(3)}case State.${node.caseName} =>\n")
    } else {
      output.append(s"${indent(5)}case Event.${node.caseName} =>\n")
    }
    node.caseActionNode.foreach(_.accept(this))
  }

  override def visit(node: FunctionCallNode): Unit = {
    output.append(s"${indent(6)}${node.functionName}(")

    if (node.argument != null)
      node.argument.foreach(_.accept(this))

    output.append(")\n")
  }

  override def visit(node: NextStateNode): Unit =
    output.append(s"State.${node.nextState}")

  override def visit(node: DefaultCaseNode): Unit =
    output.append(s"${indent(5)}case _ => unhandledTransition(state.toString, event.toString)\n")

  override def visit(node: EnumNode): Unit =
    output.append(
      s"\n${indent(1)}private enum ${node.name}:\n" +
        s"${indent(2)}case\n" +
        s"${indent(2)}" +
        s"${node.enumerators.mkString(s",\n${indent(2)}")}\n")

  @throws[IOException]
  def writeFiles(path: String, fileName: String): Unit = {
    val fullFileName = fileName + ".scala"
    val bytes = output.toString.getBytes

    val outputPath =
      if (path == null)
        FileSystems.getDefault.getPath(fullFileName)
      else
        FileSystems.getDefault.getPath(path, fullFileName)

    Files.write(outputPath, bytes)
  }

  def getOutput: String = output.toString()

  private def indent(n: Int): String = {
    val scalaSpacing = "  "
    scalaSpacing.repeat(n)
  }
}
