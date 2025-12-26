package smc.semanticAnalyzer

import scala.collection.mutable.ListBuffer

final class SemanticSyntax {
  val errors: ListBuffer[SemanticError] = ListBuffer.empty[SemanticError]
  val warnings: ListBuffer[SemanticError] = ListBuffer.empty[SemanticError]

  def addError(error: SemanticError): Unit =
    errors += error
}
