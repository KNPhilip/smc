package smc.generators.selection

trait SelectionNodeVisitor {
  def visit(node: SwitchCaseNode): Unit
  def visit(node: CaseNode): Unit
  def visit(node: DefaultCaseNode): Unit
  def visit(node: FunctionCallNode): Unit
  def visit(node: EnumNode): Unit
  def visit(node: EnumeratorNode): Unit
  def visit(node: StatePropertyNode): Unit
  def visit(node: EventDelegatorsNode): Unit
  def visit(node: HandleEventNode): Unit
  def visit(node: FsmClassNode): Unit
  def writeFiles(path: String, fileName: String): Unit
}
