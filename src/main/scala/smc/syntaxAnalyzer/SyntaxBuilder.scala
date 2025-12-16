package smc.syntaxAnalyzer

trait SyntaxBuilder {
  def newMachine(): Unit
  def setInitialState(): Unit
}
