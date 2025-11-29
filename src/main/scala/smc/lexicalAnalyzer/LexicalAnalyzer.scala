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
      val success: Boolean = {
        findWhitespace(line) ||
        findKeyword(line) ||
        findSyntaxSugar(line) ||
        findName(line)
      }

      if (!success) {
        collector.error(lineNumber, position + 1)
        position += 1
      }
    }
  }

  private def findWhitespace(line: String): Boolean =
    whitePatterns.exists { pattern =>
      pattern.findPrefixMatchOf(line.substring(position)).exists { m =>
        position += m.end
        true
      }
    }

  private def findKeyword(line: String): Boolean = {
    val rest = line.substring(position)

    keywordPattern.findPrefixMatchOf(rest).exists { m =>
      val keyword = m.matched
      dispatchKeyword(keyword)
    }
  }

  private def dispatchKeyword(keyword: String): Boolean =
    keywordHandlers.get(keyword).exists { handlerDispatch =>
      handlerDispatch(lineNumber, position)
      position += keyword.length
      true
    }

  private val keywordHandlers: Map[String, (Int, Int) => Unit] = Map(
    "$machine"    -> collector.machine,
    "$initial"    -> collector.initial,
    "$state"      -> collector.state,
    "$event"      -> collector.event,
    "$superstate" -> collector.superstate,
    "$inherits"   -> collector.inherits,
    "$entry"      -> collector.entry,
    "$exit"       -> collector.exit
  )

  private def findSyntaxSugar(line: String): Boolean = {
    val rest = line.substring(position)

    syntaxSugarPattern.findPrefixMatchOf(rest).exists { m =>
      val sym = m.matched

      syntaxSugarHandlers.get(sym).foreach { handler =>
        handler(lineNumber, position)
      }
      position += sym.length
      true
    }
  }

  private val syntaxSugarHandlers: Map[String, (Int, Int) => Unit] = Map(
    "{"  -> collector.openBrace,
    "}"  -> collector.closeBrace,
    "-"  -> collector.dash,
    "=>" -> collector.arrow,
    "->" -> collector.arrow
  )

  private def findName(line: String): Boolean = {
    val rest = line.substring(position)
    findQuotedName(rest) || findUnquotedName(rest)
  }

  private def findQuotedName(rest: String): Boolean =
    quotedName.findPrefixMatchOf(rest).exists { m =>
      val unquotedName = m.matched
        .stripPrefix("\"")
        .stripSuffix("\"")

      collector.name(lineNumber, position, unquotedName)
      position += m.end
      true
    }

  private def findUnquotedName(rest: String): Boolean =
    unquotedName.findPrefixMatchOf(rest).exists { m =>
      collector.name(lineNumber, position, m.matched)
      position += m.end
      true
    }
}