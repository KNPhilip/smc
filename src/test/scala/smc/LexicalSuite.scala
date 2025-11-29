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
}

class SyntaxSugarLexerSuite extends LexicalSuite {
  test("Lexes nothing for whitespaces") {
    assertLexed("   ", "")
  }

  test("Lexes nothing for tabs") {
    assertLexed(" \t \t  ", "")
  }

  test("Can lex correctly after whitespaces and tabs") {
    assertLexed("  $machine   \t   $state ", "MA, ST")
  }
}

class ErrorLexerSuite extends LexicalSuite {
  //  test("Lexes error on unknown keyword") {
  //    assertLexed("$mygibberishkeyword", "ER1-1")
  //  }

  test("Can lex error on empty keyword") {
    assertLexed("$", "ER1-1")
  }

  test("Can lex error on unknown syntax") {
    assertLexed(",", "ER1-1")
  }

  test("Lexes errors on multi-line unknown syntax") {
    assertLexed(".\n!", "ER1-1, ER2-1")
  }

  test("Lexes correct positional errors") {
    assertLexed(" ,  \n  ¤", "ER1-2, ER2-3")
  }

  test("Lexes correct errors on complicated unknown syntax") {
    assertLexed(".!§£\n$@<>\\_\n*^`\n´|",
      "ER1-1, ER1-2, ER1-3, ER1-4, ER2-1, " +
      "ER2-2, ER2-3, ER2-4, ER2-5, ER2-6, " +
      "ER3-1, ER3-2, ER3-3, ER4-1, ER4-2")
  }
}