package smc.lexicalAnalyzer

class LexicalAnalyzer(collector: TokenCollector) {
  def lex(input: String): Unit = {
    if (input == "$machine") {
      collector.machine(0, 0)
    }
  }
}