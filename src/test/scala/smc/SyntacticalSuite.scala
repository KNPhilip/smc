package smc

import munit.FunSuite
import smc.lexicalAnalyzer.LexicalAnalyzer
import smc.syntaxAnalyzer.SyntacticalAnalyzer

class SyntacticalSuite extends FunSuite {
  protected val parser: SyntacticalAnalyzer = new SyntacticalAnalyzer()
  private val lexer: LexicalAnalyzer = new LexicalAnalyzer(parser)

  protected def assertParsed(input: String, output: String): Unit = {
    lexer.lex(input)
    assertEquals(parser.getStateMachineSyntax.toString.trim, output)
  }
}

class SyntaxSuite extends SyntacticalSuite {
  test("My test") {
    assertParsed("$machine \"MyMachine\" {  }", "{\n  machine MyMachine \n}")
  }
}