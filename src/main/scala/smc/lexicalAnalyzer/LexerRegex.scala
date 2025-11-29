package smc.lexicalAnalyzer

import scala.util.matching.Regex

private val whitePattern: Regex   = """^\s+""".r
private val commentPattern: Regex = """^(#|//).*$""".r
val whitePatterns         = Seq(whitePattern, commentPattern)
val keywordPattern: Regex = """^\$(\w+)""".r