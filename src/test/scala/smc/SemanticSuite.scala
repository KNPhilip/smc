package smc

import munit.FunSuite
import smc.semanticAnalyzer.SemanticAnalyzer
import smc.syntaxAnalyzer.StateMachineSyntax

class SemanticSuite extends FunSuite {
  private val analyzer = new SemanticAnalyzer()

  protected def assertErrors(input: StateMachineSyntax, output: String): Unit = {
    val semanticSyntax = analyzer.analyze(input)
    val errors = semanticSyntax.errors.mkString(" ")
    assertEquals(errors, output)
  }
}

class MySuite extends SemanticSuite {
  test("No machines") {
    val syntax = new StateMachineSyntax()
    assertErrors(syntax, "NO_MACHINES")
  }
}
