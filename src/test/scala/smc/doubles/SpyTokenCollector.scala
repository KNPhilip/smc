package smc.doubles

import scala.collection.mutable.ListBuffer
import smc.lexicalAnalyzer.TokenCollector

class SpyTokenCollector extends TokenCollector {
  private val tokens = ListBuffer[String]()

  def getTokens: String = tokens.mkString(", ")
  def clear(): Unit = tokens.clear()

  override def machine(line: Int, position: Int): Unit =
    tokens += "MA"

  override def initial(line: Int, position: Int): Unit =
    tokens += "IN"

  override def state(line: Int, position: Int): Unit =
    tokens += "ST"

  override def event(line: Int, position: Int): Unit =
    tokens += "EV"

  override def abstractState(line: Int, position: Int): Unit =
    tokens += "AB"

  override def colon(line: Int, position: Int): Unit =
    tokens += "CO"

  override def entry(line: Int, position: Int): Unit =
    tokens += "EN"

  override def exit(line: Int, position: Int): Unit =
    tokens += "EX"

  override def arrow(line: Int, position: Int): Unit =
    tokens += "AR"

  override def openBrace(line: Int, position: Int): Unit =
    tokens += "OB"

  override def closeBrace(line: Int, position: Int): Unit =
    tokens += "CB"

  override def dash(line: Int, position: Int): Unit =
    tokens += "DA"

  override def name(line: Int, position: Int, value: String): Unit =
    tokens += s"NA-$value"

  override def error(line: Int, position: Int): Unit =
    tokens += s"ER-L$line-P$position"
}