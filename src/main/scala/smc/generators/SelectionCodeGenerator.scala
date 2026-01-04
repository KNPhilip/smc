package smc.generators

import smc.generators.selection.{SelectionNodeFactory, SelectionNodeVisitor}
import smc.generators.selection.visitors.ScalaVisitor
import smc.optimizer.OptimizedStateMachine

final class SelectionCodeGenerator(val language: String,
                                   val outputPath: String) extends CodeGenerator {
  override def generate(machine: OptimizedStateMachine): Unit = {
    val visitor: SelectionNodeVisitor = createVisitor()
    SelectionNodeFactory.generate(machine).accept(visitor)
    visitor.writeFiles(outputPath, machine.name)
  }

  private def createVisitor(): SelectionNodeVisitor = {
    if (language.toLowerCase == "scala") {
      new ScalaVisitor()
    } else {
      throw new IllegalArgumentException(s"Language \"$language\" is not currently supported.")
    }
  }
}
