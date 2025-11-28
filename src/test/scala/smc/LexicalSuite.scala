package smc

import munit.FunSuite
import smc.doubles.SpyTokenCollector
import smc.lexicalAnalyzer.LexicalAnalyzer

class LexicalSuite extends FunSuite {
  val spy: SpyTokenCollector = new SpyTokenCollector()
  var lexer: LexicalAnalyzer = _

  override def beforeEach(context: BeforeEach): Unit = {
    lexer = new LexicalAnalyzer(spy)
  }

  private def assertLexed(input: String, output: String): Unit = {
    lexer.lex(input)
    assertEquals(spy.getTokens, output)
  }

  test("Finds machine") {
    assertLexed("$machine", "MA")
  }
}