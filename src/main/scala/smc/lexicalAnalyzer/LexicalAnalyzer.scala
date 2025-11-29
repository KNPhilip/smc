package smc.lexicalAnalyzer

class LexicalAnalyzer(collector: TokenCollector) {
  private var lineNumber: Int = 0
  private var position: Int = 0

  def lex(input: String): Unit = {
    lineNumber = 1

    input.linesIterator.foreach { line =>
      lexLine(line)
      lineNumber += 1
    }
  }

  private def lexLine(line: String): Unit = {
    position = 0

    while (position < line.length) {
      val success: Boolean = findKeyword(line)

      if (!success) {
        collector.error(lineNumber, position + 1)
        position += 1
      }
    }
  }

  private def findKeyword(line: String): Boolean = {
    val rest = line.substring(position)

    keywordPattern.findPrefixMatchOf(rest) match {
      case Some(m) =>
        val fullKeyword = m.matched
        dispatchKeyword(fullKeyword)

      case None =>
        false
    }
  }

  private def dispatchKeyword(keyword: String): Boolean =
    keywordHandlers.get(keyword) match {
      case Some(handler) =>
        handler(lineNumber, position)
        position += keyword.length
        true

      case None =>
        false
    }

  val keywordHandlers: Map[String, (Int, Int) => Unit] = Map(
    "$machine"    -> collector.machine,
    "$initial"    -> collector.initial,
    "$state"      -> collector.state,
    "$event"      -> collector.event,
    "$superstate" -> collector.superstate,
    "$inherits"   -> collector.inherits,
    "$entry"      -> collector.entry,
    "$exit"       -> collector.exit
  )
}