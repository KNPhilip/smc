package smc.semanticAnalyzer

import smc.semanticAnalyzer.SemanticError.*
import smc.syntaxAnalyzer.StateMachineSyntax

final class SemanticAnalyzer {
  def analyze(input: StateMachineSyntax): SemanticSyntax = {
    val syntax = new SemanticSyntax()
    syntax.addError(NO_MACHINES)
    syntax
  }
}
