package smc.doubles

import smc.lexicalAnalyzer.TokenCollector
import scala.collection.mutable.ListBuffer

class SpyTokenCollector extends TokenCollector {
  private val tokens = ListBuffer[String]()

  def getTokens: String = tokens.mkString(", ")

  override def machine(line: Int, position: Int): Unit = {
    tokens += "MA"
  }

  override def initial(line: Int, position: Int): Unit = {
    tokens += "IN"
  }

  override def state(line: Int, position: Int): Unit = {
    tokens += "ST"
  }

  override def event(line: Int, position: Int): Unit = {
    tokens += "EV"
  }

  override def superstate(line: Int, position: Int): Unit = {
    tokens += "SU"
  }

  override def inherits(line: Int, position: Int): Unit = {
    tokens += "IN"
  }

  override def entry(line: Int, position: Int): Unit = {
    tokens += "EN"
  }

  override def exit(line: Int, position: Int): Unit = {
    tokens += "EX"
  }

  override def singleComment(line: Int, position: Int): Unit = {
    tokens += "SC"
  }

  override def openMultiComment(line: Int, position: Int): Unit = {
    tokens += "OM"
  }

  override def closeMultiComment(line: Int, position: Int): Unit = {
    tokens += "CM"
  }

  override def arrow(line: Int, position: Int): Unit = {
    tokens += "AR"
  }

  override def openBrace(line: Int, position: Int): Unit = {
    tokens += "OB"
  }

  override def closedBrace(line: Int, position: Int): Unit = {
    tokens += "CB"
  }

  override def dash(line: Int, position: Int): Unit = {
    tokens += "DA"
  }

  override def name(line: Int, position: Int, value: String): Unit = {
    tokens += "NA"
  }

  override def error(line: Int, position: Int): Unit = {
    tokens += "ER"
  }
}