package smc.generators

import smc.generators.nsc.{NscNodeFactory, NscNodeVisitor}
import smc.generators.nsc.visitors.ScalaVisitor
import smc.optimizer.OptimizedStateMachine

final class NscCodeGenerator(val language: String,
                             val outputPath: String) extends CodeGenerator {
  override def generate(machine: OptimizedStateMachine): Unit = {
    val visitor: NscNodeVisitor = createVisitor()
    NscNodeFactory.generate(machine).accept(visitor)
    visitor.writeFiles(outputPath, machine.name)
  }

  private def createVisitor(): NscNodeVisitor = {
    if (language.toLowerCase == "scala") {
      new ScalaVisitor()
    } else {
      throw new IllegalArgumentException(s"Language \"$language\" is not currently supported.")
    }
  }
}
