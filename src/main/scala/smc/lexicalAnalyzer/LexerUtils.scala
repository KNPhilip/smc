package smc.lexicalAnalyzer

import scala.util.matching.Regex

val keywordPattern: Regex = """^\$(\w+)""".r
