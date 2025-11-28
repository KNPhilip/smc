package smc.lexicalAnalyzer

trait TokenCollector {
  def machine(line: Int, position: Int): Unit
  def initial(line: Int, position: Int): Unit
  def state(line: Int, position: Int): Unit
  def event(line: Int, position: Int): Unit
  def superstate(line: Int, position: Int): Unit
  def inherits(line: Int, position: Int): Unit
  def entry(line: Int, position: Int): Unit
  def exit(line: Int, position: Int): Unit
  def singleComment(line: Int, position: Int): Unit
  def openMultiComment(line: Int, position: Int): Unit
  def closeMultiComment(line: Int, position: Int): Unit
  def arrow(line: Int, position: Int): Unit
  def openBrace(line: Int, position: Int): Unit
  def closedBrace(line: Int, position: Int): Unit
  def dash(line: Int, position: Int): Unit
  def name(line: Int, position: Int, value: String): Unit
  def error(line: Int, position: Int): Unit
}