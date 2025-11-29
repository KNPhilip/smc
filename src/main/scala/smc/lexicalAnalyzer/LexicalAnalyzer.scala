package smc.lexicalAnalyzer

import scala.util.matching.Regex

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
      findKeyword(line)
    }
  }

  private val keywordPattern: Regex = """^\$(\w+)""".r

  private def findKeyword(line: String): Boolean = {
    val rest = line.substring(position)

    keywordPattern.findPrefixMatchOf(rest) match {
      case Some(m) =>
        val fullKeyword = m.matched
        keywords.get(fullKeyword) match {
          case Some(handler) =>
            handler(lineNumber, position)
            position += fullKeyword.length
            true

          case None =>
            position += fullKeyword.length
            false
        }

      case None =>
        false
    }
  }

  private val keywords: Map[String, (Int, Int) => Unit] = Map(
    "$machine"   -> collector.machine,
    "$initial"  -> collector.initial,
    "$state"   -> collector.state,
    "$event"  -> collector.event,
    "$superstate"   -> collector.superstate,
    "$inherits"  -> collector.inherits,
    "$entry"        -> collector.entry,
    "$exit"       -> collector.exit
  )
}