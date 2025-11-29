package smc

import munit.FunSuite
import smc.doubles.SpyTokenCollector
import smc.lexicalAnalyzer.LexicalAnalyzer

class LexicalSuite extends FunSuite {
  protected val spy: SpyTokenCollector = new SpyTokenCollector()
  private val lexer: LexicalAnalyzer = new LexicalAnalyzer(spy)

  override def beforeEach(context: BeforeEach): Unit = {
    spy.clear()
  }

  protected def assertLexed(input: String, output: String): Unit = {
    lexer.lex(input)
    assertEquals(spy.getTokens, output)
  }
}

class KeywordLexerSuite extends LexicalSuite {
  test("Can lex the machine keyword") {
    assertLexed("$machine", "MA")
  }

  test("Can lex initial keyword") {
    assertLexed("$initial", "IN")
  }

  test("Can lex the state keyword") {
    assertLexed("$state", "ST")
  }

  test("Can lex the event keyword") {
    assertLexed("$event", "EV")
  }

  test("Can lex the superstate keyword") {
    assertLexed("$superstate", "SU")
  }

  test("Can lex the inherits keyword") {
    assertLexed("$inherits", "IH")
  }

  test("Can lex the entry keyword") {
    assertLexed("$entry", "EN")
  }

  test("Can lex the exit keyword") {
    assertLexed("$exit", "EX")
  }

//  test("Lexes error on unknown keyword") {
//    assertLexed("$mygibberishkeyword", "ER1-1")
//  }

  test("Lexes error on empty keyword") {
    assertLexed("$", "ER1-1")
  }
}