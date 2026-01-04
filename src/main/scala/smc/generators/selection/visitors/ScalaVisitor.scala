package smc.generators.selection.visitors

import java.io.IOException
import java.nio.file.{FileSystems, Files}
import smc.generators.selection._

final class ScalaVisitor extends SelectionNodeVisitor {
  private val output = new StringBuilder
  private var states: List[String] = List.empty

  override def visit(switchCaseNode: SwitchCaseNode): Unit = {
    if (switchCaseNode.variableName == "state") {
      output.append(s"${indent(2)}${switchCaseNode.variableName} match {\n")
      switchCaseNode.generateCases(this)
      output.append(s"${indent(2)}}\n")
    } else {
      output.append(s"${indent(4)}${switchCaseNode.variableName} match {\n")
      switchCaseNode.generateCases(this)
      output.append(s"${indent(4)}}\n")
    }
  }

  override def visit(caseNode: CaseNode): Unit = {
    if (states.contains(caseNode.caseName)) {
      output.append(s"${indent(3)}case State.${caseNode.caseName} =>\n")
    } else {
      output.append(s"${indent(5)}case Event.${caseNode.caseName} =>\n")
    }
    caseNode.caseActionNode.foreach(_.accept(this))
  }

  override def visit(defaultCaseNode: DefaultCaseNode): Unit =
    output.append(s"${indent(5)}case _ => unhandledTransition(state.toString, event.toString)\n")

  override def visit(functionCallNode: FunctionCallNode): Unit = {
    output.append(s"${indent(6)}${functionCallNode.functionName}(")

    if (functionCallNode.argument != null)
      functionCallNode.argument.foreach(_.accept(this))

    output.append(")\n")
  }

  override def visit(enumNode: EnumNode): Unit =
    output.append(
      s"\n${indent(1)}private enum ${enumNode.name}:\n" +
      s"${indent(2)}case\n" +
      s"${indent(2)}" +
      s"${enumNode.enumerators.mkString(s",\n${indent(2)}")}\n")

  override def visit(enumeratorNode: EnumeratorNode): Unit =
    output.append(s"${enumeratorNode.enumeration}.${enumeratorNode.enumerator}")

  override def visit(statePropertyNode: StatePropertyNode): Unit = {
    output.append(s"${indent(1)}private var state: State = State.${statePropertyNode.initialState}\n\n")
    output.append(
      s"${indent(1)}private def setState(s: State): Unit =\n" +
      s"${indent(2)}state = s\n\n")
  }

  override def visit(eventDelegatorsNode: EventDelegatorsNode): Unit =
    eventDelegatorsNode.events.foreach { event =>
      output.append(
        s"${indent(1)}def $event(): Unit =\n" +
        s"${indent(2)}handleEvent(Event.$event)\n\n")
    }

  override def visit(handleEventNode: HandleEventNode): Unit = {
    output.append(s"${indent(1)}private def handleEvent(event: Event): Unit = {\n")
    handleEventNode.switchCase.accept(this)
    output.append(s"${indent(1)}}\n")
  }

  override def visit(fsmClassNode: FsmClassNode): Unit = {
    val actionsName = fsmClassNode.actionsName
    if (actionsName == null)
      output.append(s"abstract class ${fsmClassNode.className} {\n")
    else
      output.append(s"abstract class ${fsmClassNode.className} extends $actionsName {\n")

    output.append(s"${indent(1)}def unhandledTransition(state: String, event: String): Unit\n\n")

    if (actionsName == null)
      fsmClassNode.actions.foreach { action =>
        output.append(s"${indent(1)}protected def $action(): Unit\n\n")
      }

    states = fsmClassNode.stateEnum.enumerators
    fsmClassNode.delegators.accept(this)
    fsmClassNode.stateProperty.accept(this)
    fsmClassNode.handleEvent.accept(this)
    fsmClassNode.stateEnum.accept(this)
    fsmClassNode.eventEnum.accept(this)

    output.append("}\n")
  }

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
