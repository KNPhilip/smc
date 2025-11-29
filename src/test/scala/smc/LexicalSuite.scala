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

  test("Can lex opening brace") {
    assertLexed("{", "OB")
  }

  test("Can lex closing brace") {
    assertLexed("}", "CB")
  }

  test("Can lex a dash") {
    assertLexed("-", "DA")
  }

  test("Can lex the arrow syntax") {
    assertLexed("=>", "AR")
  }

  test("Can lex alternative arrow syntax") {
    assertLexed("->", "AR")
  }

  test("Can lex a basic name") {
    assertLexed("\"HelloWorld\"", "NA-HelloWorld")
  }

  test("Can lex unquoted name") {
    assertLexed("HelloWorld", "NA-HelloWorld")
  }

  test("Can lex multiple unquoted names") {
    assertLexed("Hello World", "NA-Hello, NA-World")
  }
}

class ErrorLexerSuite extends LexicalSuite {
  test("Can lex error on unknown keyword") {
    assertLexed("$gibberish", "ER-L1-P1, NA-gibberish")
  }

  test("Can lex error on empty keyword") {
    assertLexed("$", "ER-L1-P1")
  }

  test("Can lex error on unknown syntax") {
    assertLexed(",", "ER-L1-P1")
  }

  test("Can lex errors on multi-line unknown syntax") {
    assertLexed(".\n!", "ER-L1-P1, ER-L2-P1")
  }

  test("Can lex correct positional errors") {
    assertLexed(" ,  \n  ¤", "ER-L1-P2, ER-L2-P3")
  }

  test("Can lex correct errors on complicated unknown syntax") {
    assertLexed(".!§£\n$@<>\\_\n*^`\n´|",
      "ER-L1-P1, ER-L1-P2, ER-L1-P3, ER-L1-P4, ER-L2-P1, " +
      "ER-L2-P2, ER-L2-P3, ER-L2-P4, ER-L2-P5, ER-L2-P6, " +
      "ER-L3-P1, ER-L3-P2, ER-L3-P3, ER-L4-P1, ER-L4-P2")
  }

  test("Can lex errors when spaces in name") {
    assertLexed("\"Hello World\"",
      "ER-L1-P1, NA-Hello, NA-World, ER-L1-P13")
  }

  test("Can lex error when only one quote is present") {
    assertLexed("\"HelloWorld", "ER-L1-P1, NA-HelloWorld")
  }

  test("Can lex error when combining name with unknown syntax") {
    assertLexed("Hello%World", "NA-Hello, ER-L1-P6, NA-World")
  }
}

class ComplexSyntaxLexerSuite extends LexicalSuite {
  test("Sequence of tokens") {
    assertLexed("{\"firstAction\" secondAction}",
      "OB, NA-firstAction, NA-secondAction, CB")
  }

  test("Complex sequence of tokens") {
    val input = "$machine \"fsm\" -> initial " +
      "{ $state start => doMore -> end => dispatch }"

    assertLexed(input, "MA, NA-fsm, AR, NA-initial, OB, ST, NA-start, AR, " +
      "NA-doMore, AR, NA-end, AR, NA-dispatch, CB")
  }

  test("Sequence of all keywords") {
    val input = "$machine $initial $state $event " +
      "$superstate $inherits $entry $exit"

    assertLexed(input, "MA, IN, ST, EV, SU, IH, EN, EX")
  }

  test("Sequence of all syntax sugars") {
    assertLexed("=> -> - { }", "AR, AR, DA, OB, CB")
  }

  test("Sequence with multiple lines and including errors") {
    val input = "$initial hello { => . hello \n @ } $exit"

    assertLexed(input, "IN, NA-hello, OB, AR, " +
      "ER-L1-P21, NA-hello, ER-L2-P2, CB, EX")
  }
}